module Language.Marlowe.ACTUS.HP.ContractTerms where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Time

data ContractTerm = CT | IPNR | IPDC | IED | NT | CLDR | BDC | EOMC | SD | CNTRL | FEANX | FECL | FEB | FER
  | FEAC | IPANX | IPCL | IPAC | IPCED | IPCBANX | IPCBCL | IPCB | IPCBA | MD | CDD | PDIED | PRANX | PRCL | PRNXT | PRD
  | PPRD | TD | PTD | RRMO | SCIXSD | SCANX | SCCL | SCEF | OPANX | OPCL | PYTP | PYRT | PPEF | RRANX | RRCL
  | RRSP | RRPF | RRPC | RRLF | RRLC | RRNXT | RRMLT

data EOMC = EOMC_EOM
          | EOMC_SD deriving (Show, Eq)

data BDC =  BDC_NULL
          | BDC_SCF
          | BDC_SCMF
          | BDC_CSF
          | BDC_CSMF
          | BDC_SCP
          | BDC_SCMP
          | BDC_CSP
          | BDC_CSMP deriving (Show, Eq)

data DCC =  DCC_A_AISDA
          | DCC_A_360
          | DCC_A_365
          | DCC_E30_360ISDA
          | DCC_E30_360
          | DCC_B_252 deriving (Show)

data CalendarType = NoCalendar | MondayToFriday deriving (Show)

data ContractRole = CR_RPA -- Real position asset
                  | CR_RPL -- Real position liability
                  | CR_CLO -- Role of a collateral
                  | CR_CNO -- Role of a close-out-netting
                  | CR_COL -- Role of an underlying to a collateral
                  | CR_LG  -- Long position
                  | CR_ST  -- Short position
                  | CR_BUY -- Protection buyer
                  | CR_SEL -- Protection seller
                  | CR_RFL -- Receive first leg
                  | CR_PFL -- Pay first leg
                  | CR_RF  -- Receive fix leg
                  | CR_PF  -- Pay fix leg
                  deriving (Show, Eq)

data ScalingEffect =  SE_000
                    | SE_0N0
                    | SE_00M
                    | SE_0NM
                    | SE_I00
                    | SE_IN0
                    | SE_I0M
                    | SE_INM deriving (Show, Eq)

data InterestCalculationBase = ICB_NT | ICB_NTIED | ICB_NTL deriving (Show, Eq)

data FeeBasis = FB_A | FB_N deriving (Show, Eq)

data PenaltyType = PT_O | PT_A | PT_N | PT_I deriving (Show, Eq)

data PrepaymentEffect = PE_N | PE_A | PE_M deriving (Show, Eq)

data BoundTypes = INF | SUP

data Period = P_D -- Day
            | P_W -- Week
            | P_M -- Month
            | P_Q -- Quarter
            | P_H -- Half Year
            | P_Y -- Year
            deriving (Show, Eq, Ord)

data Stub = ShortStub | LongStub deriving (Show, Eq, Ord)

data Cycle = Cycle
  { n :: Integer
  , p :: Period
  , stub :: Stub
  } deriving (Show, Eq, Ord)


{- generic representations of contract terms -}
data GenericContractTerms = GenericContractTerms {
    ratesAndSums :: Map ContractTerm [Double],
    endOfMonthConventions :: Map ContractTerm [EOMC],
    businessDayConventions :: Map ContractTerm [BDC],
    dayCountConventions :: Map ContractTerm [DCC],
    calendarTypes :: Map ContractTerm [CalendarType],
    roles :: Map ContractTerm [ContractRole],
    periods :: Map ContractTerm [Period],
    stubs :: Map ContractTerm [Stub],
    cycles :: Map ContractTerm [Cycle],
    scalingEffects :: Map ContractTerm [ScalingEffect]
}
