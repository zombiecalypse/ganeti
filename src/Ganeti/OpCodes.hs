{-# LANGUAGE ExistentialQuantification, TemplateHaskell, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Implementation of the opcodes.

-}

{-

Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014 Google Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Ganeti.OpCodes
  ( pyClasses
  , OpCode(..)
  , ReplaceDisksMode(..)
  , DiskIndex
  , mkDiskIndex
  , unDiskIndex
  , opID
  , opReasonSrcID
  , allOpIDs
  , allOpFields
  , opSummary
  , CommonOpParams(..)
  , defOpParams
  , MetaOpCode(..)
  , resolveDependencies
  , wrapOpCode
  , setOpComment
  , setOpPriority
  ) where

import Control.Applicative
import Data.List (intercalate)
import qualified Text.JSON
import Text.JSON (readJSON, JSObject, JSON, JSValue(..), fromJSObject)

import qualified Ganeti.Constants as C
import Ganeti.JSON (DictObject(..), readJSONfromDict, showJSONtoDict)
import Ganeti.OpCodes.Description
import Ganeti.OpParams
import Ganeti.PyValue ()
import Ganeti.Query.Language (queryTypeOpToRaw)
import Ganeti.THH
import Ganeti.Types

instance PyValue DiskIndex where
  showValue = showValue . unDiskIndex

instance PyValue IDiskParams where
  showValue _ = error "OpCodes.showValue(IDiskParams): unhandled case"

instance PyValue RecreateDisksInfo where
  showValue RecreateDisksAll = "[]"
  showValue (RecreateDisksIndices is) = showValue is
  showValue (RecreateDisksParams is) = showValue is

instance PyValue a => PyValue (SetParamsMods a) where
  showValue SetParamsEmpty = "[]"
  showValue _ = error "OpCodes.showValue(SetParamsMods): unhandled case"

instance PyValue a => PyValue (NonNegative a) where
  showValue = showValue . fromNonNegative

instance PyValue a => PyValue (NonEmpty a) where
  showValue = showValue . fromNonEmpty

-- FIXME: should use the 'toRaw' function instead of being harcoded or
-- perhaps use something similar to the NonNegative type instead of
-- using the declareSADT
instance PyValue ExportMode where
  showValue ExportModeLocal = show C.exportModeLocal
  showValue ExportModeRemote = show C.exportModeLocal

instance PyValue CVErrorCode where
  showValue = cVErrorCodeToRaw

instance PyValue VerifyOptionalChecks where
  showValue = verifyOptionalChecksToRaw

instance PyValue INicParams where
  showValue = error "instance PyValue INicParams: not implemented"

instance PyValue a => PyValue (JSObject a) where
  showValue obj =
    "{" ++ intercalate ", " (map showPair (fromJSObject obj)) ++ "}"
    where showPair (k, v) = show k ++ ":" ++ showValue v

instance PyValue JSValue where
  showValue (JSObject obj) = showValue obj
  showValue x = show x

-- * Generate all opcodes used in the haskell code base.
$(genOpCodeDecl "OpCode" opCodeDescriptions)
$(genOpCode "OpCode" opCodeDescriptions)

deriving instance Ord OpCode

-- | Returns the OP_ID for a given opcode value.
$(genOpID ''OpCode "opID")

-- | A list of all defined/supported opcode IDs.
$(genAllOpIDs ''OpCode "allOpIDs")

-- | Convert the opcode name to lowercase with underscores and strip
-- the @Op@ prefix.
$(genOpLowerStrip (C.opcodeReasonSrcOpcode ++ ":") ''OpCode "opReasonSrcID")

instance JSON OpCode where
  readJSON = readJSONfromDict
  showJSON = showJSONtoDict

-- | Generates the summary value for an opcode.
opSummaryVal :: OpCode -> Maybe String
opSummaryVal OpClusterVerifyGroup { opGroupName = s } = Just (fromNonEmpty s)
opSummaryVal OpGroupVerifyDisks { opGroupName = s } = Just (fromNonEmpty s)
opSummaryVal OpClusterRename { opName = s } = Just (fromNonEmpty s)
opSummaryVal OpQuery { opWhat = s } = Just (queryTypeOpToRaw s)
opSummaryVal OpQueryFields { opWhat = s } = Just (queryTypeOpToRaw s)
opSummaryVal OpNodeRemove { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpNodeAdd { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpNodeModifyStorage { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpRepairNodeStorage  { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpNodeSetParams { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpNodePowercycle { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpNodeMigrate { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpNodeEvacuate { opNodeName = s } = Just (fromNonEmpty s)
opSummaryVal OpInstanceCreate { opInstanceName = s } = Just s
opSummaryVal OpInstanceReinstall { opInstanceName = s } = Just s
opSummaryVal OpInstanceRemove { opInstanceName = s } = Just s
-- FIXME: instance rename should show both names; currently it shows none
-- opSummaryVal OpInstanceRename { opInstanceName = s } = Just s
opSummaryVal OpInstanceStartup { opInstanceName = s } = Just s
opSummaryVal OpInstanceShutdown { opInstanceName = s } = Just s
opSummaryVal OpInstanceReboot { opInstanceName = s } = Just s
opSummaryVal OpInstanceReplaceDisks { opInstanceName = s } = Just s
opSummaryVal OpInstanceFailover { opInstanceName = s } = Just s
opSummaryVal OpInstanceMigrate { opInstanceName = s } = Just s
opSummaryVal OpInstanceMove { opInstanceName = s } = Just s
opSummaryVal OpInstanceConsole { opInstanceName = s } = Just s
opSummaryVal OpInstanceActivateDisks { opInstanceName = s } = Just s
opSummaryVal OpInstanceDeactivateDisks { opInstanceName = s } = Just s
opSummaryVal OpInstanceRecreateDisks { opInstanceName = s } = Just s
opSummaryVal OpInstanceSetParams { opInstanceName = s } = Just s
opSummaryVal OpInstanceGrowDisk { opInstanceName = s } = Just s
opSummaryVal OpInstanceChangeGroup { opInstanceName = s } = Just s
opSummaryVal OpGroupAdd { opGroupName = s } = Just (fromNonEmpty s)
opSummaryVal OpGroupAssignNodes { opGroupName = s } = Just (fromNonEmpty s)
opSummaryVal OpGroupSetParams { opGroupName = s } = Just (fromNonEmpty s)
opSummaryVal OpGroupRemove { opGroupName = s } = Just (fromNonEmpty s)
opSummaryVal OpGroupEvacuate { opGroupName = s } = Just (fromNonEmpty s)
opSummaryVal OpBackupPrepare { opInstanceName = s } = Just s
opSummaryVal OpBackupExport { opInstanceName = s } = Just s
opSummaryVal OpBackupRemove { opInstanceName = s } = Just s
opSummaryVal OpTagsGet { opKind = s } = Just (show s)
opSummaryVal OpTagsSearch { opTagSearchPattern = s } = Just (fromNonEmpty s)
opSummaryVal OpTestDelay { opDelayDuration = d } = Just (show d)
opSummaryVal OpTestAllocator { opIallocator = s } =
  -- FIXME: Python doesn't handle None fields well, so we have behave the same
  Just $ maybe "None" fromNonEmpty s
opSummaryVal OpNetworkAdd { opNetworkName = s} = Just (fromNonEmpty s)
opSummaryVal OpNetworkRemove { opNetworkName = s} = Just (fromNonEmpty s)
opSummaryVal OpNetworkSetParams { opNetworkName = s} = Just (fromNonEmpty s)
opSummaryVal OpNetworkConnect { opNetworkName = s} = Just (fromNonEmpty s)
opSummaryVal OpNetworkDisconnect { opNetworkName = s} = Just (fromNonEmpty s)
opSummaryVal _ = Nothing

-- | Computes the summary of the opcode.
opSummary :: OpCode -> String
opSummary op =
  case opSummaryVal op of
    Nothing -> op_suffix
    Just s -> op_suffix ++ "(" ++ s ++ ")"
  where op_suffix = drop 3 $ opID op

-- | Generic\/common opcode parameters.
$(buildObject "CommonOpParams" "op"
  [ pDryRun
  , pDebugLevel
  , pOpPriority
  , pDependencies
  , pComment
  , pReason
  ])

deriving instance Ord CommonOpParams

-- | Default common parameter values.
defOpParams :: CommonOpParams
defOpParams =
  CommonOpParams { opDryRun     = Nothing
                 , opDebugLevel = Nothing
                 , opPriority   = OpPrioNormal
                 , opDepends    = Nothing
                 , opComment    = Nothing
                 , opReason     = []
                 }

-- | Resolve relative dependencies to absolute ones, given the job ID.
resolveDependsCommon :: (Monad m) => CommonOpParams -> JobId -> m CommonOpParams
resolveDependsCommon p@(CommonOpParams { opDepends = Just deps}) jid = do
  deps' <- mapM (`absoluteJobDependency` jid) deps
  return p { opDepends = Just deps' }
resolveDependsCommon p _ = return p

-- | The top-level opcode type.
data MetaOpCode = MetaOpCode { metaParams :: CommonOpParams
                             , metaOpCode :: OpCode
                             } deriving (Show, Eq, Ord)

-- | Resolve relative dependencies to absolute ones, given the job Id.
resolveDependencies :: (Monad m) => MetaOpCode -> JobId -> m MetaOpCode
resolveDependencies mopc jid = do
  mpar <- resolveDependsCommon (metaParams mopc) jid
  return (mopc { metaParams = mpar })

instance DictObject MetaOpCode where
  toDict (MetaOpCode meta op) = toDict meta ++ toDict op
  fromDictWKeys dict = MetaOpCode <$> fromDictWKeys dict
                                  <*> fromDictWKeys dict

instance JSON MetaOpCode where
  readJSON = readJSONfromDict
  showJSON = showJSONtoDict

-- | Wraps an 'OpCode' with the default parameters to build a
-- 'MetaOpCode'.
wrapOpCode :: OpCode -> MetaOpCode
wrapOpCode = MetaOpCode defOpParams

-- | Sets the comment on a meta opcode.
setOpComment :: String -> MetaOpCode -> MetaOpCode
setOpComment comment (MetaOpCode common op) =
  MetaOpCode (common { opComment = Just comment}) op

-- | Sets the priority on a meta opcode.
setOpPriority :: OpSubmitPriority -> MetaOpCode -> MetaOpCode
setOpPriority prio (MetaOpCode common op) =
  MetaOpCode (common { opPriority = prio }) op
