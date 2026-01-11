namespace Numberlink.ZigZag.Core.Lib

open System

type Domain<'s when 's : comparison> = Map<'s, float>

module Domain =
    /// Returns true if the domain contains no states.
    let isEmpty domain =
        Map.isEmpty domain

    /// Returns the number of states in the domain.
    let count domain =
        Map.count domain

    /// Returns the sum of all weights in the domain.
    let total domain =
        Map.fold (fun acc _ w -> acc + w) 0.0 domain

    /// Returns the Shannon entropy of the domain, or None if empty.
    let entropy domain =
        if isEmpty domain then None
        else
            domain
            |> Map.toSeq
            |> Seq.sumBy (fun (_, weight) ->
                let total = total domain
                let p = weight / total
                if p > 0.0 then -p * log p else 0.0)
            |> Some

    /// Returns true if the domain contains exactly one state.
    let isSingleton domain =
        count domain = 1

    /// Returns the single state in the domain, assuming the domain is a singleton.
    let getSingleton domain =
        domain
        |> Map.toSeq
        |> Seq.head
        |> fst

    /// Returns Some state if the domain is a singleton, otherwise None.
    let trySingleton domain =
        if isSingleton domain then Some (getSingleton domain)
        else None

    /// Returns a random state from the domain, with probabilities determined by their weights.
    let observe (random: Random) domain =
        let rec pick threshold remaining =
            match remaining with
            | [] -> None
            | [(state, _)] -> Some state
            | (state, weight) :: rest ->
                if threshold <= weight then Some state
                else pick (threshold - weight) rest

        domain
        |> Map.toList
        |> pick (random.NextDouble() * total domain)
        
    /// Returns all states in weighted-random order as a list.
    let observeAll random domain =
        let rec loop acc domain =
            match observe random domain with
            | None -> List.rev acc
            | Some state -> loop (state :: acc) (Map.remove state domain)

        loop [] domain

type Constraint<'v, 'e, 's when 's : comparison> =
    Guid -> Domain<'s> -> Map<Guid, 's> -> Graph<'v, 'e> -> Domain<'s>

type WaveFunctionCollapse<'v, 's, 'e when 's : comparison> = {
    Random: Random
    Domains: Map<Guid, Domain<'s>>
    Collapsed: Map<Guid, 's>
    Constraints: Constraint<'v, 'e, 's> list
    Graph: Graph<'v, 'e>
}

module WaveFunctionCollapse =
    /// Initialize a new wave function collapse instance.
    let init random initialDomains constraints graph = {
        Random = random
        Domains = initialDomains graph
        Collapsed = Map.empty
        Constraints = constraints
        Graph = graph
    }

    /// Select the uncollapsed vertex with the lowest entropy, breaking ties randomly.
    let tryPickLowestEntropy wfc =
        let candidates =
            wfc.Domains
            |> Map.toList
            |> List.choose (fun (id, domain) -> 
                Domain.entropy domain |> Option.map (fun e -> id, e))

        match candidates with
        | [] -> None
        | _ ->
            let minEntropy = candidates |> List.map snd |> List.min
            let ties = candidates |> List.filter (fun (_, e) -> e = minEntropy) |> List.map fst
            Some (List.item (wfc.Random.Next(List.length ties)) ties)

    /// Apply all constraints to compute the valid domain for a vertex.
    let applyConstraints vertexId wfc =
        let initialDomain =
            wfc.Domains
            |> Map.tryFind vertexId
            |> Option.defaultValue Map.empty

        wfc.Constraints
        |> List.fold (fun domain constraint' -> 
            constraint' vertexId domain wfc.Collapsed wfc.Graph) initialDomain
        |> Map.filter (fun _ weight -> weight > 0.0)

    /// Propagate constraints after collapsing vertices, returning None if a contradiction is found.
    let propagate newlyCollapsed wfc =
        let wfc = { wfc with Collapsed = Map.foldBack Map.add newlyCollapsed wfc.Collapsed }

        let initialQueue =
            newlyCollapsed
            |> Map.toList
            |> List.collect (fun (id, _) -> Graph.getNeighborIds id wfc.Graph)

        let rec loop queue processed wfc =
            match queue with
            | [] -> 
                Some wfc

            | id :: rest when Set.contains id processed || wfc.Collapsed.ContainsKey id ->
                loop rest processed wfc

            | id :: rest ->
                let domain = applyConstraints id wfc

                if Domain.isEmpty domain then
                    None
                else
                    let processed = Set.add id processed

                    match Domain.trySingleton domain with
                    | Some state ->
                        let wfc = { wfc with
                                        Domains = Map.remove id wfc.Domains
                                        Collapsed = Map.add id state wfc.Collapsed }

                        let newNeighbors =
                            Graph.getNeighborIds id wfc.Graph
                            |> List.filter (fun n -> 
                                not (wfc.Collapsed.ContainsKey n) && 
                                not (Set.contains n processed))

                        loop (newNeighbors @ rest) processed wfc

                    | None ->
                        let wfc = { wfc with Domains = Map.add id domain wfc.Domains }
                        loop rest processed wfc

        loop initialQueue Set.empty wfc

    /// Collapse all vertices that have only one possible state remaining.
    let collapseGuaranteed wfc =
        let guaranteed =
            wfc.Domains
            |> Map.toList
            |> List.choose (fun (id, domain) ->
                Domain.trySingleton domain |> Option.map (fun state -> id, state))
            |> Map.ofList

        if Map.isEmpty guaranteed then 
            Some wfc
        else
            let wfc = { wfc with Domains = Map.filter (fun id _ -> not (Map.containsKey id guaranteed)) wfc.Domains }
            propagate guaranteed wfc

    /// Execute the wave function collapse algorithm with backtracking.
    let run wfc =
        let rec solve stack wfc =
            match tryPickLowestEntropy wfc with
            | None -> 
                Some wfc.Collapsed
            | Some vertexId ->
                let domain = Map.find vertexId wfc.Domains
                let choices = Domain.observeAll wfc.Random domain
                tryChoices stack wfc vertexId choices

        and tryChoices stack wfc vertexId choices =
            match choices with
            | [] -> 
                backtrack stack
            | choice :: remainingChoices ->
                let newStack = 
                    if List.isEmpty remainingChoices then stack 
                    else (wfc, vertexId, remainingChoices) :: stack

                match propagate (Map.ofList [vertexId, choice]) wfc with
                | Some wfc' -> solve newStack wfc'
                | None -> tryChoices stack wfc vertexId remainingChoices

        and backtrack stack =
            match stack with
            | [] -> None
            | (wfc, vertexId, choices) :: rest -> 
                tryChoices rest wfc vertexId choices

        collapseGuaranteed wfc |> Option.bind (solve [])
