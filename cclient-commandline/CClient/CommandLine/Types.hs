module CClient.CommandLine.Types where

type Url          = String
type Uuid         = String
type TextAppend   = String
type Probability  = Double

data Task = Get Url Probability | Done




