namespace dvMENTALmadness.Core

// see: http://fsharpforfunandprofit.com/posts/printf/
module Logging =

    open log4net

    let debug (logger:ILog) s =
        logger.Debug(s)

    let debugf (logger:ILog) format =
        let doAfter s =
            logger.Debug(s)
        Printf.ksprintf doAfter format

    let info (logger:ILog) s =
        logger.Info(s)

    let infof (logger:ILog) format =
        let doAfter s =
            logger.Info(s)
        Printf.ksprintf doAfter format

    let warn (logger:ILog) s =
        logger.Warn(s)

    let warnf (logger:ILog) format =
        let doAfter s =
            logger.Warn(s)
        Printf.ksprintf doAfter format

    let error (logger:ILog) ex s =
        logger.Error(s, ex)

    let errorf (logger:ILog) ex format =
        let doAfter s =
            logger.Error(s, ex)
        Printf.ksprintf doAfter format
    
    let fatal (logger:ILog) ex s =
        logger.Fatal(s, ex)

    let fatalf (logger:ILog) ex format =
        let doAfter s =
            logger.Fatal(s, ex)
        Printf.ksprintf doAfter format



