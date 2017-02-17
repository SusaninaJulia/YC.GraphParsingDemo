namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("YC.GraphParsingDemo")>]
[<assembly: AssemblyProductAttribute("YC.GraphParsingDemo")>]
[<assembly: AssemblyDescriptionAttribute("Web demo for graph parser")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
    let [<Literal>] InformationalVersion = "1.0"
