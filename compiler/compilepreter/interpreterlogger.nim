## Module defines the core logging facility for the compilepretter(s)

type
  ## various identifiers to point at various pieces of data
  # xxx: need to incorporate once we get to environments
  RunId* = distinct int32
    ## valid values are 0 and above, some state tracking uses values outside
    ## this range (negative numbers), for special casing, if runId was ever
    ## set, the first run ever, etc.

  PhaseId* {.size: sizeof(uint8).} = enum
    phaseFirst    ## whatever `FirstInterpreter` produces
                  # xxx: kinda implies interpreter == level

  UntypedEvtTag* = uint16
    ## the erased event/outstruction enum value, must be sized to 16 bits max

  OutstrStatus* {.pure, size: sizeof(uint8).} = enum
    ## common to all events/outstructions, since everything is meant to be
    ## transactional, this gives a single way to demarcate them consistently.
    started
    successful ## finished successfullly
    failed     ## finished, with errors
    cancelled  ## stopped by the interpreter
    aborted    ## stopped by the caller

  LogEntry* = object
    ## the log needs to be writeable by any interpter, so rather than figuring
    ## out everything upfront in one mega-module, keep a standardize structure
    ## and allow interpreters/modules own their definitions (schema on read).
    # xxx: alternative approach is to change this to an index type and the
    #      actual log is a big ol' byte array which we index into and cast to
    #      pull out variably sized data... not sure what to do, this _seems_
    #      easier to implement now and hopefully similarly easy to work with.
    run*: RunId              # reserving space, ignoring for now
    phase*: PhaseId
    evtStatus*: OutstrStatus
    evtTag*: UntypedEvtTag
    data*: uint64            ## the data, identifier, or index; interpretation
                             ## depends upon the `phase`/`status`/`evtTag`

  InterpreterLogger* = object
    ## logger for the interpreters, we use a single format, extra data is in
    ## look aside buffers that need to be queried at time of read.
    data: seq[LogEntry] # xxx: consider making an array list style structure

# logger interface/impl start

proc startEvt*(logger: var InterpreterLogger, run: RunId, phase: PhaseId,
               tag: UntypedEvtTag, data: uint64) =
  logger.data.add LogEntry(run: run, phase: phase, evtStatus: started,
                           evtTag: tag, data: data)

proc closeEvt*(logger: var InterpreterLogger, run: RunId, phase: PhaseId,
               status: range[successful..aborted], tag: UntypedEvtTag,
               data: uint64) =
  logger.data.add LogEntry(run: run, phase: phase, evtStatus: status,
                           evtTag: tag, data: data)