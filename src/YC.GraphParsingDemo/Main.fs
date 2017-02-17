namespace YC.GraphParsingDemo

open WebSharper
open WebSharper.Sitelets

type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/about">] About
    | [<EndPoint "/graph"; Wildcard>] Graph of countOfVertex:int * edges: array<int * int * string * int>

module Templating =
    open WebSharper.Html.Server

    type Page =
        {
            Title : string
            Body : list<Element>
        }

    type GraphPage =
        {
            Title : string
            Body : list<Element>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let GraphTemplate =
        Content.Template<GraphPage>("~/Graph.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let Main ctx endpoint title body : Async<Content<EndPoint>> =
        Content.WithTemplate MainTemplate
            {
                Title = title
                Body = body
            }

    let Graph title body =
        Content.WithTemplate GraphTemplate
            {
                Title = title
                Body = body
            }

module Site =
    open WebSharper.Html.Server

    let HomePage ctx =
        Templating.Main ctx EndPoint.Home "Home" [
            Div [ClientSide <@ Client.Main() @>]
        ]
    let GraphPage g i =
        Templating.Graph "Graph" [
            Div [Attr.Id "canvas"; Attr.Height "height"; Attr.Width "width"]
        ]

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | EndPoint.Home -> HomePage ctx
            | EndPoint.Graph (i, g) -> GraphPage g i

        )
