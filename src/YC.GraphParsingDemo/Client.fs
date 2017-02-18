namespace YC.GraphParsingDemo

open WebSharper.Formlets

open WebSharper
open WebSharper.JavaScript
open WebSharper.Html.Client


module wsfc = WebSharper.Formlets.Controls
module wsfe = WebSharper.Formlets.Enhance
module wsfd = WebSharper.Formlets.Data
module wsff = WebSharper.Formlets.Formlet
module wsfl = WebSharper.Formlets.Layout


[<JavaScript>]
module Client =

    let screenWidth = JQuery.JQuery.Of("html").Width()
    let screenHeight = JQuery.JQuery.Of("html").Height()

    let getFormSize (height: int) (width: int) = 
        ((height * screenHeight / 638).ToString() + "px", (width * screenWidth / 1366).ToString() + "px")

    let setFormSize ((height: string), (width: string)) (formletType: string) (formlet: Formlets.Data.Formlet<'c>) =
        formlet |> wsff.MapElement (fun e ->
            JQuery.JQuery.Of(e.Dom.QuerySelector(formletType))
                .Css("height", height) 
                .Css("width", width)
                .Ignore
            e)
    
    let style = "padding-top: 0px; background-color: #FF69B4; border-width: 3px; font-weight: bold; border-color: #000000; border-radius: 10px; color: #000000; height: " + fst(getFormSize 40 150) + "; width: " + snd(getFormSize 40 150) + "; font-size:" + fst(getFormSize 15 15);                                                                                              

    let ChooseDefaultControl (defaultData: List<string * string>) = 
        wsff.Do {
            let! dataSelect = 
                wsfc.Select 0 (("", "") :: defaultData)
                |> wsfe.WithTextLabel "ChooseDefault"
                |> setFormSize (getFormSize 30 210) "select" 
                |> wsfe.WithFormContainer     
            return dataSelect }
 
    let FileControl = 
        let readFile (elFrom: Element) (stateChanged: Event<_>) =
            let file = (WebSharper.JavaScript.FileList.OfElement elFrom.Dom).Item 0
            let reader = new WebSharper.JavaScript.TextFileReader()            
            reader.ReadAsText file
            reader.AddEventListener("load", (fun () -> stateChanged.Trigger(Result.Success reader.Result)), true)

        Formlet.BuildFormlet <| fun() ->
            let stateChanged = new Event<Result<string>>()
            let input =
                Input [Attr.Type "file"; Attr.Accept "text/*"]
                |>! OnChange (fun e -> readFile e stateChanged)                        
            let reset () =
                input.Value <- ""
                stateChanged.Trigger(Result.Success "")
            input, reset, stateChanged.Publish
        |> Formlet.InitWith ""
    
    let RangeControl =
        wsff.Yield (fun min max -> (int min, int max))
        <*> wsff.Do {
                let! initVert = 
                    wsfc.Input ""
                    |> wsfe.WithTextLabel "Initial" 
                    |> setFormSize (getFormSize 20 50) "input"
                return initVert }
        <*> wsff.Do {                
                let! finVert = 
                    wsfc.Input ""
                    |> wsfe.WithTextLabel "Final" 
                    |> setFormSize (getFormSize 20 50) "input"      
                return finVert }
        |> wsff.Horizontal 
        |> wsfe.WithTextLabel "Vertices"
        |> wsfe.WithLabelAbove
        |> wsfe.WithFormContainer
    
    let ErrorControl errortxt lbl = 
        wsff.Do {
        let! output =
            wsff.OfElement (fun () -> TextArea [Attr.ReadOnly "readonly"; Text ("Error: " + errortxt)])
            |> wsfe.WithTextLabel lbl
            |> wsfe.WithLabelAbove
            |> setFormSize (getFormSize 90 540) "textarea" 
        return output }
        |> wsfe.WithFormContainer 

    let Graph lbl (height, width, g: array<int*int*string*bool>, c: int) =
        let hw = "height: " + snd(getFormSize 540 90) + "; width: " + fst(getFormSize 540 90)
        let button = Button [Text lbl; Attr.Style hw] 
        button.OnClick (fun _ _ -> 
            JS.Window?draw1 g c
            button.Remove()) 
        Div [
            Div [Attr.Id "canvas"; Attr.Height height; Attr.Width width]
            button
            ]

    let SPPF lbl (height, width, g: array<int*string*int*string>, c: int) =
        let hw = "height: " + snd(getFormSize 540 90) + "; width: " + fst(getFormSize 540 90)
        let button = Button [Text lbl; Attr.Style hw]
        button.OnClick (fun _ _ -> 
            JS.Window?draw2 g c
            button.Remove()) 
        Div [
            Div [Attr.Id "canvas"; Attr.Height height; Attr.Width width; ]
            button
            ]
    let ShowGraphImageControl lbl (graph: Parser.InputGraph) = 
        wsff.OfElement(fun () -> Graph lbl ((fst(getFormSize 540 540)), (snd(getFormSize 540 540)), graph.edges, graph.countOfVertex))
        |> wsfe.WithLabelAbove 
        |> wsfe.WithFormContainer  

    let ShowTreeImageControl lbl  (tree: Parser.ParsedSppf)  = 
        wsff.OfElement(fun () -> SPPF lbl ((fst(getFormSize 540 540)), (snd(getFormSize 540 540)), tree.edges, tree.countOfVertex)) 
        |> wsfe.WithLabelAbove 
        |> wsfe.WithFormContainer          
                                         
    let InputGrammarControl lbl defaultData = 
       wsff.Do {
            let! (defaultValue, fileInput) = 
                wsff.Do {  
                    let! defaultValue = ChooseDefaultControl defaultData
                    let! fileInput = FileControl
                    return  (fileInput, defaultValue) }   
                |> wsff.Horizontal                      
                |> wsfe.WithFormContainer              
            let txt = 
                match fileInput with
                | "" -> defaultValue
                | _ -> fileInput
            let! textInput =
                wsfc.TextArea txt            
                |> wsfe.WithTextLabel lbl
                |> wsfe.WithLabelAbove
                |> setFormSize (getFormSize 137 540) "textarea"     // 137 - form is bigger, but both on the same level     
            return (textInput) }
         |> wsff.FlipBody
         |> wsff.Vertical
         |> wsfe.WithFormContainer

    let InputGraphControl lbl defaultData = 
       wsff.Do {
            let! (defaultValue, fileInput) = 
                wsff.Do {  
                    let! defaultValue = ChooseDefaultControl defaultData
                    let! fileInput = FileControl
                    return  (defaultValue, fileInput) }   
                |> wsff.Horizontal                      
                |> wsfe.WithFormContainer              
            let txt = 
                match fileInput with
                | "" -> defaultValue
                | _ -> fileInput
            let! textInput =
                wsfc.TextArea txt            
                |> wsfe.WithTextLabel lbl
                |> wsfe.WithLabelAbove
                |> setFormSize (getFormSize 90 540) "textarea"          
            return (textInput) }
         |> wsff.FlipBody
         |> wsff.Vertical
         |> wsfe.WithFormContainer
    let Form = 
        let InputForm =   
            let LeftInputForm =         
                wsff.Do {
                    let! grammar = InputGrammarControl "Grammar" (Server.LoadDefaultFileNames Server.FileType.Grammar |> List.map (fun grmName -> grmName, Server.LoadDefaultFile Server.FileType.Grammar grmName))
                    return (grammar) }
                |> wsff.Vertical
            let RightInputForm  = 
                wsff.Do {
                    let! graph = InputGraphControl "Graph" (Server.LoadDefaultFileNames Server.FileType.Graph |> List.map (fun grmName -> grmName, Server.LoadDefaultFile Server.FileType.Graph grmName))
                    let! subgraphCheckbox = wsfc.Checkbox false |> wsfe.WithTextLabel "Show formal subgraph" |> wsfe.WithLabelLeft// |> wsfe.WithFormContainer
                    let! removeCheckbox = wsfc.Checkbox false |> wsfe.WithTextLabel "Remove redundant nodes" |> wsfe.WithLabelLeft//|> wsfe.WithFormContainer 
                    return(graph,subgraphCheckbox,removeCheckbox)}        
                |> wsff.Vertical
                |> wsfe.WithFormContainer
            
            (wsff.Yield (fun (leftInput: string) (rightInput: string*bool*bool) -> (leftInput,  rightInput))
            <*> (LeftInputForm)
            <*> (RightInputForm))
            |> wsff.Horizontal
            |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
                                                                                            Label = Some "SHOW GRAPH" 
                                                                                            Style = Some style })
                
        let OutputForm ((grammar: string), ((graph: string), (removeCheckbox: bool),  (subgraphCheckbox: bool))) =  
            let VisualizationWithRangeForm = 
                let VisualizationForm  =                               
                        wsff.Do {
                            match Server.draw grammar graph removeCheckbox subgraphCheckbox with
                            | Server.Result.Error msg ->
                                let! graphImg = ErrorControl msg "Graph Visualization"
                                let! sppfImg = ErrorControl msg "SPPF"
                                return (graphImg, sppfImg)
                            | Server.Result.SucTreeGraph (tree, graph) ->
                                let! graphImg = ShowGraphImageControl "Graph Visualization" graph
                                let! sppfImg = ShowTreeImageControl  "SPPF" tree
                                return (graphImg, sppfImg) }          
                          |> wsfe.WithFormContainer 
                          |> wsff.Horizontal  

                let RangeAndButtonForm  =
                        wsff.Do {
                            let! rng = RangeControl
                            return rng }                   
                        |> wsfe.WithCustomSubmitButton ({ wsfe.FormButtonConfiguration.Default with 
                                                                                                    Label = Some "FIND PATH"
                                                                                                    Style = Some style })   
                        |> wsff.Horizontal    
               
                wsff.Do {
                    let! x = VisualizationForm 
                    let! y = RangeAndButtonForm
                    return (x, y) }
                |> wsff.Vertical
            
            let PathsVisualizationForm rng = 
                        wsff.Do {
                            if fst rng < snd rng && fst rng >= 0 && snd rng >= 0
                            then
                                match Server.findMinLen grammar graph removeCheckbox (fst rng) (snd rng) with
                                | Server.Result.Error msg ->
                                    let! pathImg = ErrorControl msg "Path"
                                    let! sppfPathImg = ErrorControl msg "SPPF Path" 
                                    return (pathImg, sppfPathImg)
                                | Server.Result.SucTreeGraph (tree, graph) ->
                                    let! pathImg = ShowGraphImageControl "Path" graph
                                    let! sppfPathImg = ShowTreeImageControl "SPPF Path" tree
                                    return (pathImg, sppfPathImg)
                            else
                                let! pathImg = ErrorControl "Incorrect range" "Path"
                                let! sppfPathImg = ErrorControl "Incorrect range" "SPPF Path"
                                return (pathImg, sppfPathImg) } 
                        |> wsfe.WithFormContainer 
                        |> wsff.Horizontal 
            
            wsff.Do {
                let! x = VisualizationWithRangeForm 
                let! y = PathsVisualizationForm (snd x)
                return (x, y) }
                |> wsff.Vertical  
                     
        wsff.Do {
            let! x = InputForm 
            let! y = OutputForm x
            return (x, y) }
            |> wsff.Vertical 
    
    let Main () =
        let MainForm =
            Form.Run(fun _ -> ())
        
        Div [      
           MainForm
        ]