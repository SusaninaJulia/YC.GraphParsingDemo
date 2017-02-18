namespace YC.GraphParsingDemo

open WebSharper
open Yard.Frontends.YardFrontend.Main
open Yard.Generators.GLL.AbstractParser
open AbstractAnalysis.Common
open Yard.Generators.GLL
open Yard.Generators.Common.FinalGrammar
open Yard.Generators.Common.InitialConvert
open Yard.Generators.Common.ASTGLL
open Yard.Generators.GLL.ParserCommon
open System.Collections.Generic
open Yard.Generators.GLL
open Yard.Generators.Common

module Server = 

    type Result =
        | SucTreeGraph of Parser.ParsedSppf * Parser.InputGraph
        | Error of string

    type FileType =
        | Graph
        | Grammar

    [<Rpc>]
    let LoadDefaultFileNames (fileType: FileType) =
        match fileType with
        | Grammar ->
            [
                "Math"
                "2"
            ]
        | Graph ->
            [
                "Math"
                "2"
            ]
    [<Rpc>]
    let LoadDefaultFile (fileType: FileType) name =
        match fileType with
        | Grammar ->
            match name with
            | "1" -> @"[<Start>]
s: s P n | n
n: n M y | y
y: L s R | INT"
            | "2" -> @"[<Start>]
s: a b | b c | d
a: A
b: C
c: G
d: U"
            |  _  -> ""
        | Graph ->
            match name with
            | "1" -> @"digraph {
    0 -> 1 [label = L]
    1 -> 2 [label = INT]
    2 -> 3 [label = P]
    3 -> 4 [label = INT]
    1 -> 5 [label = INT]
    5 -> 6 [label = M]
    6 -> 7 [label = INT]
    7 -> 8 [label = P]
    8 -> 4 [label = INT]
    4 -> 9 [label = R]
    9 -> 10 [label = M]
    10 -> 11 [label = INT]
    11 -> 12 [label = P]
    12 -> 13 [label = INT]            
}"
            | "2" -> @"digraph {
    0 -> 1 [label = A]
    1 -> 2 [label = C]            
}"
            |  _  -> ""

    [<Rpc>]
    let draw (grammar'text : string) (graph'text : string) (isFormal : bool) (isMinimised : bool)=
        try
            if grammar'text = "" && graph'text = "" then Error "Empty input"
            elif graph'text = "" then Error "Empty graph input"
            elif grammar'text = "" then Error "Empty grammar input"
            else
                let grammar, graph = Parser.grmParse grammar'text, Parser.graphParse graph'text
                match Parser.parse grammar graph with
                | Yard.Generators.GLL.ParserCommon.ParseResult.Error msg -> Error msg
                | Yard.Generators.GLL.ParserCommon.ParseResult.Success tree ->
                    if isMinimised
                    then
                        if isFormal
                        then
                            let minimisedTree = Parser.minimiseSppf tree
                            let formalSubgraph = Parser.getFormalSubgraph minimisedTree (Parser.graphToMap graph)
                            if formalSubgraph.countOfVertex <> 0
                            then
                                SucTreeGraph(Parser.treeToParsed minimisedTree minimisedTree.Root (fun x -> true), formalSubgraph)
                            else
                                Error "There is no verticles in subgraph"
                        else
                            let minimisedTree = Parser.minimiseSppf tree
                            SucTreeGraph (Parser.treeToParsed minimisedTree minimisedTree.Root (fun x -> true), Parser.toInputGraph graph)
                    else
                        if isFormal
                        then
                            let formalSubgraph = Parser.getFormalSubgraph tree (Parser.graphToMap graph)
                            if formalSubgraph.countOfVertex <> 0
                            then
                                SucTreeGraph(Parser.treeToParsed tree tree.Root (fun x -> true), formalSubgraph)
                            else
                                Error "There is no verticles in subgraph"
                        else
                            SucTreeGraph (Parser.treeToParsed tree tree.Root (fun x -> true), Parser.toInputGraph graph)
        with
        | e -> Error e.Message

    [<Rpc>]
    let findMinLen (grammar'text : string) (graph'text : string) (isMinimised : bool) (first : int) (second : int) =
        try
            if grammar'text = "" && graph'text = "" then Error "Empty input"
            elif graph'text = "" then Error "Empty graph input"
            elif grammar'text = "" then Error "Empty grammar input"
            else
                let grammar, graph = Parser.grmParse grammar'text, Parser.graphParse graph'text
                match Parser.parse grammar graph with
                | Yard.Generators.GLL.ParserCommon.ParseResult.Error msg -> Error msg
                | Yard.Generators.GLL.ParserCommon.ParseResult.Success tree ->
                    let mtree =
                        if isMinimised
                        then
                            Parser.minimiseSppf tree
                        else
                            tree
                    let nTNode = Parser.getNonTermNode mtree (packExtension first second)
                    match nTNode with
                    | Parser.ResNode.Suc(node) ->
                        let edges, nodes = Parser.getEdgesOfMinLen node
                        let tree, graph = Parser.getTreeOfMnLn mtree edges nodes node (Parser.toInputGraph graph)
                        SucTreeGraph(tree, graph)
                    |  Parser.ResNode.None -> 
                        Error "No such nodes found"
                    |  Parser.ResNode.Error msg -> 
                        Error msg
        with
        |e -> Error e.Message