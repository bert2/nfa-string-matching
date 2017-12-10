namespace Proc

module Proc =

    open System.Collections.Generic
    open System.Diagnostics

    let run filename args = 
        let proc = 
            new Process (
                StartInfo = 
                    ProcessStartInfo (
                        FileName = filename,
                        Arguments = args,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false
                    )
            )

        let output = List<string> ()
        let handler (_sender:obj) (args:DataReceivedEventArgs) = 
            if args.Data <> null 
            then output.Add args.Data
        proc.OutputDataReceived.AddHandler (DataReceivedEventHandler handler)
        proc.ErrorDataReceived.AddHandler (DataReceivedEventHandler handler)
        
        let started = proc.Start ()
        if not started then failwithf "Failed to start process %s" filename
        proc.BeginOutputReadLine ()
        proc.BeginErrorReadLine ()
        proc.WaitForExit ()
        
        proc.ExitCode, List.ofSeq output