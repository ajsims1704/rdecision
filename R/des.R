# ---------------------------------------------------------------------------
# des.R
#
# Description:
# ============
# Solves Markov model using discrete event simulation Monte-Carlo.
#
# History:
# ========
# 17.04.2012. A.J. Sims. Created.
# 18.04.2012. A.J. Sims. Added discounting.
# 16.10.2013. A.J. Sims. Converted to R.
# 01.01.2014. A.J. Sims. Added to R package and updated documentation.
# 03.01.2014. A.J. Sims. Added utility to state data frame.
# 24.07.2017. A.J. Sims. Use write.csv to create -pop, -psp and -ent files.
#                        Added ability to restart simulation.
# 05.01.2018. A.J. Sims. Several changes:
#                          Fixed argument list;
#                          Added probability sum to state tables;
#                          Fixed retention of accumulated costs and utilities
#                            after restarts
#                          Incurred entry costs for initial distribution
#                          Discounted costs for cycles 2 onwards
# 18.01.2018. A.J. Sims. Changes:
#                          Added cycle to populate states and incur entry costs
#                          Post-cycle limit algorithm loops until all patients
#                           in states with expired cycle limits have moved;
#                           i.e. correct behaviour for cycleLimit=0
#                          Fixed bug in discount rate on restarts.
# 19.01.2018. A.J. Sims. Changes:
#                          Attribute 0.5 occupancy cost and 0.5 utility based
#                           on state at start of cycle, then 0.5 occupancy cost
#                           and 0.5 utility based on state at end of cycle.
#                          Print pre-cycle populations in summary table.
# 23.01.2018. A.J. Sims. Changes:
#                          Added group structure, allowing maximum occupany of
#                           any of the states in a group to be defined. Useful
#                           for implants with finite lifetime.
# 14.05.2019. A.J. Sims. Changes:
#                          Renamed to des (discrete event simulation).
#                          Handled case of nGroups=0
# 23.05.2019. A.J. Sims. Changes:
#                          Default stub is NA (no output generated), which is
#                          helpful for package vignette usage.
#
# Limitations:
# ============
#   - Output becomes ragged for large cohorts where total costs are high;
#     this could be changed to show per-patient costs, or expressed as
#     thousands, millions etc.
#   - Writing a calling script to take model inputs and convert them to
#     the arguments of this function can be messy. It would be better to
#     separate the call into validate, print model, run cycles, print output
#     etc.
#   - At present the only use for groups is to define a maximum occupancy
#     of a patient in one of a group of states. Defining
#     group$hasCycleLimit=FALSE is currently a legal but pointless
#     exercise.
# ----------------------------------------------------------------------------

#' @title Discrete event simulation solver of Markov models
#'
#' @description \code{des} is solver function for the \code{rdecision}
#' package for discrete event simulations. The function is based on the model
#' described by Sonnenberg
#' and Beck. This function solves a Markov state model using a Monte-Carlo
#' method, based on random number generation.
#'
#' @references
#' Sonnenberg FA and Beck JR, "Markov models in medical decision
#' making: a practical guide", Medical Decision Making 1993;13:322-339.
#' @param nStates Number of states in Markov model
#' @param nGroups Number of groups (default zero); states can be grouped
#'                together and a maximum cycle limit set on group occupancy.
#' @param nPatients Number of patients to simulate
#' @param nCyclesPerYear Number of cycles per year
#' @param nCycles Number of time cycles to simulate
#' @param state Data frame of length nStates with named fields (see: State)
#' @param group Data frame of length nGroups with named fields (see: Group)
#' @param prevalence Array of length nStates which contains values for the
#'                   initial state occupancy (prevalence). All values should
#'                   be in the interval [0,1] and the sum of the elements
#'                   should be 1.
#' @param Ip Matrix of ANNUAL rates of transition between states. The
#'           dimensions are (nStates,nStates) with each entry being the
#'           annual rate of transitions from the row state to the
#'           column state. Values are in the range [0,1]. Transitions
#'           to self (i.e. leading diagonal) should NOT be included.
#' @param Tp Matrix of dimension (nStates,nStates) which contain time
#'           independent probabilities of transitions from time limited
#'           states to other states after time expiry. Values are in
#'           the interval [0,1].
#' @param Gp Matrix of dimension (nGroups, nStates) which contain time
#'           independent probabilities of transitions from time limited
#'           groups to other states after time expiry. Values are in
#'           the interval [0,1]. It is an error if any of the transitions
#'           from a group to a state within that group are > 0.
#' @param discount Annual discount rate for costs and benefits as a
#'                 percentage figure, eg 3.5. This is applied to each time
#'                 cycle with log correction to convert from annual rate to
#'                 per-cycle rate if required. Default 0.00.
#' @param stub File stub to be used for output files, eg 'mymarkov'
#'             would cause files called 'mymarkov-log.txt' etc to be
#'             created. Default NA means no files are written.
#' @param continue If no, states are populated according to the given
#'             prevalence; if yes, the simulation restarts in its
#'             previous state by reading in the csv files, and the
#'             prevalences are ignored. If stub is NA (file output is
#'             suppressed) continue=T is disallowed.
#' @return A list of 4 matrices, for state populations, costs, entries
#'        and utilities.
#' @note The following files will be created by the function:
#' 'stub'-log.txt: a text file listing inputs, progress and final results
#'                 of the simulation.
#' 'stub'-pop.csv: a comma separated value file containing the population
#'                 of each state at the end of each cycle.
#' 'stub'-psp.csv: a comma separated value file containing the cumulative
#'                 cost associated with entry and occupancy of each state
#'                 after each cycle.
#' 'stub'-ent.csv: a comma separated value file containing the cumulative
#'                 number of entries into each state, after each cycle.
#' 'stub'-utl.csv: a comma separated value file containing the cumulative
#'                 utlity of each state, after each cycle.
#'
#' @section State:
#' Data frame \code{state} should have the following fields:
#' \code{name} string which describes state (used in log output)
#' \code{hasCycleLimit} value TRUE if cycle limit applies (ie temporary/tunnel
#'                      state); FALSE otherwise (ie absorbing or normal state)
#' \code{cycleLimit} (integer) maximum number of cycles for which a
#'                   single patient can occupy the state, if
#'                   hasCycleLimit = TRUE. For temporary
#'                   states use hasCycleLimit=TRUE and cycleLimit=0.
#' \code{annualCost} cost of being in the state for 1 year
#' \code{entryCost} one-off cost associated with entering the state
#' \code{utility} (incremental) utility of being in the state for one year
#' \code{group} group number to which this state belongs (0 if no group,
#'              >= 1 otherwise)
#'
#' @section Group:
#' Date frame \code{group} should have the following fields:
#' \code{number} (integer) group number as an integer (referenced
#'                by state$group); NA if not defined.
#' \code{name} string which describes the group name (in output)
#' \code{hasCycleLimit} value TRUE if cycle limit applies to the
#'                        group, FALSE otherwise.
#' \code{cycleLimit} maximum number of cycles for which a single
#'                   patient can occupy the group.
#'
#' @export
des <- function(nStates, nGroups=0, nPatients, nCyclesPerYear, nCycles,
                state, group=NA, prevalence, Ip, Tp, Gp=NA,
                discount=0.0, stub=NA, continue=F) {

  # --------------------------------------------------------------------
  # check arguments
  # --------------------------------------------------------------------
  if (is.na(stub) && continue==T) {
    stop("Continuation mode not supported when file creation is suppressed")
  }
    
  # --------------------------------------------------------------------
  # open log file
  # --------------------------------------------------------------------
  logfile <- ifelse(
    is.na(stub), 
    ifelse(.Platform$OS.type=="windows", "nul:", "/dev/null"),
# R >3.6.0    nullfile(), 
    paste(stub, '-log.txt', sep="")
  )
  
  if (!continue) {
    ofid <- file(logfile, open="wt")
  }
  else {
    ofid <- file(logfile, open="at")
    fprintf(ofid, "\n\n(restarting)\n\n")
  }

  # ------------------------------------------------------------------
  # initialise RAND to a different state each time...
  # ------------------------------------------------------------------
  set.seed(seed=as.numeric(Sys.time()), kind='Mersenne-Twister')

  # ------------------------------------------------------------------
  # calculate probabilities from incidences...
  # ------------------------------------------------------------------
  P <- matrix(data=0, nrow=nStates, ncol=nStates)
  for (fromState in 1 : nStates) {
    for (toState in 1 : nStates) {
      r <- Ip[fromState, toState]
      if (is.na(r)) {r <- 0.0}
      if (nCyclesPerYear > 1) {
        p <- 1.0 - exp(-r/nCyclesPerYear)
      }
      else {
        p <- r
      }
      P[fromState, toState] <- p
    }
  }

  # -----------------------------------------------------------------
  # calculate self-return probabilities...
  # -----------------------------------------------------------------
  for (s in 1:nStates) {
    Pjump <- sum(P[s,])
    P[s,s] <- 1 - Pjump
  }

  # -------------------------------------------------------------------
  # calculate cumulative transition probabilities for each state...
  # -------------------------------------------------------------------
  Pcdf <- matrix(data=0, nrow=nStates, ncol=nStates)
  for (s in 1:nStates) {
    Pcdf[s,] <- cumsum(P[s,])
  }

  # --------------------------------------------------------------------
  # calculate cumulative transition probabilities from temp states...
  # --------------------------------------------------------------------
  Tcdf <- matrix(data=0, nrow=nStates, ncol=nStates)
  for (s in 1:nStates) {
    Tcdf[s,] <- cumsum(Tp[s,])
  }

  # --------------------------------------------------------------------
  # calculate cumulative transition probabilities for groups
  # --------------------------------------------------------------------
  if (nGroups > 0) {
    Gcdf <- matrix(data=0, nrow=nGroups, ncol=nStates)
    for (g in 1:nGroups) {
      Gcdf[g,] <- cumsum(Gp[g,])
    }
  }

  # ---------------------------------------------------------------------
  # Calculate per-cycle discount(d value)...
  # ---------------------------------------------------------------------
  r <- discount / 100.0                 # express as a proportion
  if (nCyclesPerYear > 1) {
    dCycle = 1.0 - exp(-r/nCyclesPerYear)
  }
  else {
    dCycle = r
  }

  # ---------------------------------------------------------------------
  # print model description
  # ---------------------------------------------------------------------
  fprintf(ofid, '%s', '\n')
  fprintf(ofid, '%s', 'Model description\n')
  fprintf(ofid, '%s', '=================\n')

  fprintf(ofid, '%s', '\n')
  fprintf(ofid, '%s', 'States and costs:\n')
  fprintf(ofid, '(state) %-25s %15s %15s %15s %15s\n', 'name',
                'entry cost (#)', 'annual cost (#)', 'utility', 'group')
  for (s in 1:nStates) {
    fprintf(ofid, '(%5i) %-25s %15.2f %15.2f %15.2f %15i\n', s,
                    as.character(state$name[s]),
                    state$entryCost[s], state$annualCost[s],
                    state$utility[s], state$group[s])
  }
  fprintf(ofid, '\n')

  fprintf(ofid, 'Annual discount rate = %.2f %%\n', discount)
  fprintf(ofid, '\n')

  # Annual transition probabilities
  fprintf(ofid, 'Annual transition probabilities (%%):\n')
  fprintf(ofid, '%7s   ', '')
  for (s in 1:nStates) {
    fprintf(ofid, '%7i ', s)
  }
  fprintf(ofid, '\n')
  for (sr in 1:nStates) {
    fprintf (ofid, '%7i : ', sr)
    for (sc in 1:nStates) {
      fprintf(ofid, '%7.3f ', 100.0*Ip[sr,sc])
    }
    fprintf(ofid, '\n')
  }
  fprintf (ofid, '\n')

  # Per cycle transition probabilities
  fprintf(ofid, 'Per cycle transition probabilities (%%):\n')
  fprintf (ofid, '(with %i cycles per year)\n', nCyclesPerYear)
  fprintf (ofid, '%7s   ', '')
  for (s in 1:nStates) {
    fprintf(ofid, '%7i ', s)
  }
  fprintf(ofid, "%7s", 'Sum')
  fprintf(ofid, '\n')
  for (sr in 1:nStates) {
    fprintf(ofid, '%7i : ', sr)
    for (sc in 1:nStates) {
      fprintf(ofid, '%7.3f ', 100.0*P[sr,sc])
    }
    fprintf(ofid, '%7.3f', 100.0*Pcdf[sr, nStates])
    fprintf(ofid, '\n')
  }
  fprintf (ofid, '\n')

  # Transition state probabilities
  fprintf(ofid, 'Transition state transition probabilities (%%):\n')
  fprintf(ofid, '%7s   ', '')
  for (s in 1:nStates) {
    fprintf(ofid, '%7i ', s)
  }
  fprintf(ofid, "%7s", 'Sum')
  fprintf(ofid, '\n')
  for (sr in 1:nStates) {
    fprintf (ofid, '%7i : ', sr)
    for (sc in 1:nStates) {
      fprintf(ofid, '%7.3f ', 100.0*Tp[sr,sc])
    }
    fprintf(ofid, "%7.3f", 100.0*Tcdf[sr, nStates])
    fprintf(ofid, '\n')
  }
  fprintf (ofid, '\n')

  # print cycle limits
  fprintf(ofid, 'Cycle limits for transition states:\n')
  fprintf(ofid, '%7s : %7s %7s\n', 'State', 'Limit?', 'Cycles')
  for (sr in 1:nStates) {
    fprintf(ofid, '%7i : ', sr)
    fprintf(ofid, '%7s ', ifelse(state$hasCycleLimit[sr], 'Yes', 'No'))
    fprintf(ofid, '%7i', ifelse(state$hasCycleLimit[sr], state$cycleLimit[sr], NA))
    fprintf(ofid, '\n')
  }
  fprintf (ofid, '\n')

  # print group and cycle limit details
  if (nGroups > 0) {
    fprintf(ofid, 'Group structure:\n')
    fprintf(ofid, '%7s : %-25s %7s %7s\n', 'Group', 'Name', 'Limit?', 'Cycles')
    for (g in 1:nGroups) {
      fprintf(ofid, '%7i : ', group$number[g])
      fprintf(ofid, '%-25s ', group$name[g])
      fprintf(ofid, '%7s ', ifelse(group$hasCycleLimit[g], 'Yes', 'No'))
      fprintf(ofid, '%7i', ifelse(group$hasCycleLimit[g], group$cycleLimit[g], NA))
      fprintf(ofid, '\n')
    }
    fprintf (ofid, '\n')
  }

  # print group-to-state transition probabilities
  if (nGroups > 0) {
    fprintf(ofid, 'Group-to-state transition probabilities (%%):\n')
    fprintf(ofid, '%7s   ', '')
    for (s in 1:nStates) {
      fprintf(ofid, '%7i ', s)
    }
    fprintf(ofid, "%7s", 'Sum')
    fprintf(ofid, '\n')
      for (g in 1:nGroups) {
        fprintf (ofid, '%7i : ', g)
        for (sc in 1:nStates) {
          fprintf(ofid, '%7.3f ', 100.0*Gp[g,sc])
        }
        fprintf(ofid, "%7.3f", 100.0*Gcdf[g, nStates])
        fprintf(ofid, '\n')
      }
    fprintf (ofid, '\n')
  }

  # ---------------------------------------------------------------------
  # model validation
  # ---------------------------------------------------------------------
  fprintf(ofid, '%s', '\n')
  fprintf(ofid, '%s', 'Model validation\n')
  fprintf(ofid, '%s', '================\n')

  # normal/absorbing states
  fprintf(ofid, "Errors in normal/absorbing states:\n")

  # check that annual transition rates are in range [0,1]
  for (s in 1:nStates) {
    nz <- which(Ip[s,] < 0)
    if (length(nz)>0) fprintf(ofid, " *Error: state %i has annual transition probabilities < 0*\n", s)
    nz <- which(Ip[s,] > 1)
    if (length(nz)>0) fprintf(ofid, " *Error: state %i has annual transition probabilities > 1*\n", s)
  }

  # check that sum of outgoing probabilities for all states is 1
  nz <- which(Pcdf[,nStates] > 0)
  if (any(Pcdf[nz,nStates] < 1.0)) {
    fprintf(ofid, " *Warning: Transition probabilities for some states are incomplete*\n")
  }
  fprintf(ofid, '%s', '\n')
  nz <- which(Pcdf[,nStates] > 0)
  if (any(Pcdf[nz,nStates] > 1.0)) {
    fprintf(ofid, " *Warning: Transition probabilities for some states exceed 1*\n")
  }
  fprintf(ofid, '%s', '\n')

  # temporary/tunnel states
  fprintf(ofid, "Errors in temporary/tunnel states:\n")

  # check that temporary state transition rates are in range [0,1]
  for (s in 1:nStates) {
    nz <- which(Tp[s,] < 0)
    if (length(nz)>0) fprintf(ofid, " *Error: state %i has transition probabilities < 0*\n", s)
    nz <- which(Tp[s,] > 1)
    if (length(nz)>0) fprintf(ofid, " *Error: state %i has transition probabilities > 1*\n", s)
  }

  # check that sum of outgoing probabilities is 1
  nz <- which(Tcdf[,nStates] > 0)
  if (any(Tcdf[nz, nStates] < 1.0)) {
    fprintf(ofid, " *Warning: Transition probabilities for some states are incomplete*\n")
  }
  nz <- which(Tcdf[,nStates] > 0)
  if (any(Tcdf[nz,nStates] > 1.0)) {
    fprintf(ofid, " *Warning: Transition probabilities for some states exceed 1*\n")
  }
  fprintf(ofid, '\n')

  # --------------------------------------------------------------------
  # create matrices to store information for each cycle
  # --------------------------------------------------------------------
  if (!continue) {
    # create matrix to store details of each patient
    patient <- matrix(data=0, nrow=nPatients, ncol=6);
    colnames(patient) <- c('state', 'timeInState', 'group', 'timeInGroup', 'cost', 'utility')

    # create a cumulative state occupancy and transition counter
    cumEntries <- matrix(data=0, nrow=nStates, ncol=1)
    cumOccupancies <- matrix(data=0, nrow=nStates, ncol=1)

    # create discounted cost tables
    stateCost <- matrix(data=0, nrow=nStates, ncol=3)
    colnames(stateCost) <- c('discountEntryCost', 'discountCycleCost', 'total')
    for (s in 1:nStates) {
      stateCost[s, 'discountEntryCost'] <- state$entryCost[s]
      stateCost[s, 'discountCycleCost'] <- state$annualCost[s] / nCyclesPerYear
      stateCost[s, 'total'] = 0.0
    }

    # create discounted utility tables
    stateUtility <- matrix(data=0, nrow=nStates, ncol=2)
    colnames(stateUtility) <- c('discountCycleUtility', 'total')
    for (s in 1:nStates) {
      stateUtility[s,'discountCycleUtility'] <- state$utility[s] / nCyclesPerYear
      stateUtility[s, 'total'] <- 0.0
    }
  }
  else {
    # read internal variables from file
    load(file=paste(stub, '.Rdata', sep=""))

    # update per cycle costs for restarts, but retain cumulative cost
    stateCost[s, 'discountEntryCost'] <- state$entryCost[s]
    stateCost[s, 'discountCycleCost'] <- state$annualCost[s] / nCyclesPerYear

    # update per cycle utilities for restarts, but retain cumulative utility
    stateUtility[s,'discountCycleUtility'] <- state$utility[s] / nCyclesPerYear
  }

  # --------------------------------------------------------------------
  # get current state populations
  # --------------------------------------------------------------------
  pop <- tabulate(patient[,'state'], nStates)

  # --------------------------------------------------------------------
  # print population at restart
  # --------------------------------------------------------------------
  if (continue) {
    fprintf(ofid, '\nRestarted state populations:\n')
    for (s in 1:nStates) {
      fprintf(ofid, '(%5i) %-25s: %6i\n', s, state$name[s], pop[s])
    }
  }
  fprintf(ofid, '\n')

  # --------------------------------------------------------------------
  # Output matrices for population, costs, utilities and entries
  # --------------------------------------------------------------------
  if (!continue) {
    popm <- matrix(data=NA, nrow=nCycles, ncol=nStates+2)
    colnames(popm) <- c("Cycle", as.character(state$name), "Total")
    pspm <- matrix(data=NA, nrow=nCycles, ncol=nStates+2)
    colnames(pspm) <- c("Cycle", as.character(state$name), "Total")
    entm <- matrix(data=NA, nrow=nCycles, ncol=nStates+2)
    colnames(entm) <- c("Cycle", as.character(state$name), "Total")
    utlm <- matrix(data=NA, nrow=nCycles, ncol=nStates+2)
    colnames(utlm) <- c("Cycle", as.character(state$name), "Total")
    prevCycles <- 0
  }
  else {
    popm <- as.matrix(read.csv(file=paste(stub, '-pop.csv', sep="")))
    prevCycles <- max(popm[,'Cycle'])
    popm <- rbind(popm, matrix(data=NA, nrow=nCycles, ncol=nStates+2))
    pspm <- as.matrix(read.csv(file=paste(stub, '-psp.csv', sep="")))
    pspm <- rbind(pspm, matrix(data=NA, nrow=nCycles, ncol=nStates+2))
    entm <- as.matrix(read.csv(file=paste(stub, '-ent.csv', sep="")))
    entm <- rbind(entm, matrix(data=NA, nrow=nCycles, ncol=nStates+2))
    utlm <- as.matrix(read.csv(file=paste(stub, '-utl.csv', sep="")))
    utlm <- rbind(entm, matrix(data=NA, nrow=nCycles, ncol=nStates+2))
  }

  # --------------------------------------------------------------------
  # main cycles
  # --------------------------------------------------------------------

  # calculate start and end cycle numbers
  startCycle <- ifelse(prevCycles==0, 0, prevCycles+1)
  endCycle <- prevCycles + nCycles

  # main cycle loop
  for (c in startCycle:endCycle) {

    # clear entry and occupancy accumulators
    cycEntries <- matrix(data=0, nrow=nStates, ncol=1) # in-cycle entries
    cycOccupancies <- matrix(data=0, nrow=nStates, ncol=1) # in-cycle occupancies

    # apply discounting to state costs
    denom = (1 + dCycle)^c
    for (s in 1:nStates) {
      stateCost[s,'discountEntryCost'] <- state$entryCost[s] / denom
      stateCost[s,'discountCycleCost'] <- state$annualCost[s] / (denom*nCyclesPerYear)
    }

    # apply discounting to utilities
    for (s in 1:nStates) {
      stateUtility[s,'discountCycleUtility'] <- state$utility[s] / (denom*nCyclesPerYear)
    }

    # save state populations so half cycle correction can be applied
    popBefore <- tabulate(patient[,'state'], nbins=nStates)

    # accumulate occupancies and utilities assuming starting state is 0.5 cycles
    if (c > 0) {
      for (p in 1:nPatients) {
        s <- patient[p, 'state']
        cycOccupancies[s] <- cycOccupancies[s] + 0.5
        patient[p,'cost'] <- patient[p,'cost'] + 0.5*stateCost[s,'discountCycleCost']
        patient[p,'utility'] <- patient[p,'utility'] + 0.5*stateUtility[s, 'discountCycleUtility']
      }
    }

    # update state populations based on transitions, or prevalence if cycle 0
    if (c==0) {
      prevalenceCDF <- cumsum(prevalence)
      for (p in 1:nPatients) {
        r <- runif(1, 0, 1)
        for (s in 1:nStates) {
          if (r < prevalenceCDF[s]) {
            patient[p,'state'] <- s
            patient[p,'timeInState'] <- 0
            patient[p,'group'] <- state$group[s]
            patient[p,'timeInGroup'] <- 0
            patient[p,'cost'] <- stateCost[s, 'discountEntryCost']
            cycEntries[s] <- cycEntries[s] + 1
            break
          }
        }
      }
    }
    else {
      # assign patients to new states, based on RNG
      for (p in 1:nPatients) {
        sCurrent <- patient[p,'state']
        sNew <- sCurrent
        r <- runif(1, min=0, max=1)
        for (s in 1:nStates) {
          if (r < Pcdf[sCurrent,s]) {
            sNew <- s
            break
          }
        }
        if (sNew != sCurrent) {
          patient[p,'state'] <- sNew
          patient[p,'timeInState'] <- 1
          g <- state$group[sNew]
          if (patient[p,'group'] != g) {
            patient[p,'group'] <- g
            patient[p,'timeInGroup'] <- 1
          }
          else {
            patient[p,'timeInGroup'] <- patient[p,'timeInGroup'] + 1
          }
          patient[p,'cost'] <- patient[p,'cost'] + stateCost[sNew,'discountEntryCost']
          cycEntries[sNew] <- cycEntries[sNew] + 1
        }
        else {
          patient[p,'timeInState'] <- patient[p,'timeInState'] + 1
          patient[p,'timeInGroup'] <- patient[p,'timeInGroup'] + 1
        }
      }
    }

    # save temporary state populations
    popPreLimit <- tabulate(patient[,'state'], nbins=nStates)

    # if any patients in T states have outstayed their welcome, or if
    # they have reached a group timeout, move
    # them to the next state; keep doing this until all patients who
    # have outstayed have been moved. This is necessary in case of
    # patients being moved into states with zero occupancy time.
    repeat {
      nOutstayed <- 0
      for (p in 1:nPatients) {
        sCurrent <- patient[p,'state']
        gCurrent <- patient[p,'group']
        sNew <- sCurrent

        # test for reaching state cycle limit
        if (state$hasCycleLimit[sCurrent]) {
          if (patient[p,'timeInState'] >= state$cycleLimit[sCurrent]) {
            r <- runif(1, min=0, max=1)
            for (s in 1:nStates) {
              if (r < Tcdf[sCurrent,s]) {
                sNew <- s
                break
              }
            }
          }
        }

        # test for reaching group cycle limit
        if (gCurrent > 0 && group$hasCycleLimit[gCurrent]) {
          if (patient[p,'timeInGroup'] >= group$cycleLimit[gCurrent]) {
            r <- runif(1, min=0, max=1)
            for (s in 1:nStates) {
              if (r < Gcdf[gCurrent,s]) {
                sNew <- s
                break
              }
            }
          }
        }

        # if state has changed, update the patient details
        if (sNew != sCurrent) {
          nOutstayed <- nOutstayed + 1
          patient[p,'state'] <- sNew
          patient[p,'timeInState'] <- 0
          g <- state$group[sNew]
          if (patient[p,'group'] != g) {
            patient[p,'group'] <- g
            patient[p,'timeInGroup'] <- 0
          }
          patient[p,'cost'] <- patient[p,'cost'] + stateCost[sNew, 'discountEntryCost']
          cycEntries[sNew] <- cycEntries[sNew] + 1
        }
      }
      if (nOutstayed == 0) {
        break
      }
    }

    # accumulate occupancies and utilities assuming end state is 0.5 cycles
    if (c > 0) {
      for (p in 1:nPatients) {
        s <- patient[p, 'state']
        cycOccupancies[s] <- cycOccupancies[s] + 0.5
        patient[p,'cost'] <- patient[p,'cost'] + 0.5*stateCost[s,'discountCycleCost']
        patient[p,'utility'] <- patient[p,'utility'] + 0.5*stateUtility[s, 'discountCycleUtility']
      }
    }

    # add cycle entries and occupancies to cumulative ones
    if (c > 0) {
      for (s in 1:nStates) {
        cumEntries[s] <- cumEntries[s] + cycEntries[s]
        cumOccupancies[s] <- cumOccupancies[s] + cycOccupancies[s]
      }
    }

    # save population after reassignment...
    popAfter <- tabulate(patient[,'state'], nbins=nStates)

    # apply half cycle correction...
    popHCC <- matrix(data=0, nrow=nStates, ncol=1)
    for (s in 1:nStates) {
       popHCC[s] <- (popBefore[s] + popAfter[s])/2.0
    }

    # print cycle results
    fprintf(ofid, '\n')
    fprintf(ofid, 'After cycle %3i\n', c)
    fprintf(ofid, '===============\n')

    # print discount factor
    fprintf(ofid, 'Discount factor = %.3f\n', 1/denom)
    fprintf(ofid, '\n')

    # print state populations
    fprintf(ofid, 'State populations\n')
    fprintf(ofid, '(state) %-25s: %9s %9s %9s %9s %11s %9s\n', 'name',
                  'preCycle', 'preLimit', 'postLimit',
                  'HCC', 'cum entries', 'occupancy')
    for (s in 1:nStates) {
       fprintf(ofid, '(%5i) %-25s: %9i %9i %9i %9.1f %11i %9.1f\n', s,
                      state$name[s], popBefore[s], popPreLimit[s],
                      popAfter[s], popHCC[s],
                      cumEntries[s], cumOccupancies[s])
    }
    fprintf(ofid, '(total) %-25s: %9i %9i %9i %9.1f %11s %9.1f\n',
                  ' ', sum(popBefore), sum(popPreLimit),
                  sum(popAfter),
                  sum(popHCC), ' ', sum(cumOccupancies))
    fprintf(ofid, '\n');

    # print cycle costs to log file and build cumulative costs
    fprintf(ofid, 'Cycle costs\n')
    fprintf(ofid, '(state) (%22s) + (%22s) = %14s (%14s)\n', 'entries',
                  'occupancies', '# (cycle)', '# (to date)')
    sumCycle = 0;
    sumGrandCost = 0;
    for (s in 1:nStates) {
      sumState <- cycEntries[s]*stateCost[s,'discountEntryCost'] +
                  cycOccupancies[s]*stateCost[s,'discountCycleCost']
      sumCycle <- sumCycle + sumState
      stateCost[s,'total'] <- stateCost[s,'total'] + sumState
      sumGrandCost <- sumGrandCost + stateCost[s,'total']
      fprintf(ofid, '(%5i) (%9i @ %10.2f) + (%9.1f @ %10.2f) = %14.2f (%14.2f)\n',
                    s, cycEntries[s], stateCost[s,'discountEntryCost'],
                    cycOccupancies[s], stateCost[s,'discountCycleCost'],
                    sumState, stateCost[s,'total'])
    }
    fprintf(ofid, '(%5s)  %22s     %22s  = %14.2f (%14.2f)\n',
                  'total', '', '', sumCycle, sumGrandCost);
    fprintf(ofid, '\n');

    # print utilities to log file...
    fprintf(ofid, 'Cycle utilities\n')
    fprintf(ofid, '(state) (%22s)  %14s  %14s\n',
            'occupancies', '(cycle)', '(to date)')
    sumCycle = 0;
    sumGrandUtility = 0;
    for (s in 1:nStates) {
      sumState <- cycOccupancies[s]*stateUtility[s,'discountCycleUtility']
      sumCycle <- sumCycle + sumState
      stateUtility[s,'total'] <- stateUtility[s,'total'] + sumState
      sumGrandUtility <- sumGrandUtility + stateUtility[s,'total']
      fprintf(ofid, '(%5i) (%9.1f @ %10.2f) = %13.2f (%13.2f)\n',
              s, cycOccupancies[s], stateUtility[s,'discountCycleUtility'],
              sumState, stateUtility[s,'total'])
    }
    fprintf(ofid, '(%5s) %24s   %13.2f (%13.2f)\n',
            'total', '', sumCycle, sumGrandUtility);
    fprintf(ofid, '\n');

    # write to population csv matrix
    popm[c,] <- c(c, popHCC, sum(popHCC))

    # write to entries csv
    entm[c,] <- c(c, cumEntries, sum(cumEntries))

    # write to cost csv matrix
    pspm[c,] <- c(c, stateCost[,'total'], sumGrandCost)
    
    # write to utility csv matrix
    utlm[c,] <- c(c, stateUtility[,'total'], sumGrandUtility)
  }

  # ----------------------------------------------------------------------
  # print out the costs incurred...
  # ----------------------------------------------------------------------

  # print header...
  fprintf(ofid, '\n')
  fprintf (ofid, 'Final states and costs\n')
  fprintf (ofid, '======================\n')

  fprintf(ofid, '\n')
  fprintf(ofid, 'Total costs after %i cycles (%5.2f years)\n', nCycles+prevCycles,
           (nCycles+prevCycles) / nCyclesPerYear)
  fprintf(ofid, '===========================================\n')

  # sum of costs tallied by patient...
  fprintf(ofid, '\n')
  fprintf(ofid, 'Sum of costs tallied by patient\n')
  fprintf(ofid, '-------------------------------\n')
  costTotal <- sum(patient[,'cost'])
  fprintf(ofid, '%-20s %14.2f (%s)\n', 'Total', costTotal, '#')
  fprintf(ofid, '%-20s %14.2f (%s)\n', 'Total/patient',
                 costTotal/nPatients, '#')

  # sum of utilities tallied by patient...
  fprintf(ofid, '\n')
  fprintf(ofid, 'Sum of utilities tallied by patient\n')
  fprintf(ofid, '-----------------------------------\n')
  utilityTotal <- sum(patient[,'utility'])
  fprintf(ofid, '%-20s %10.2f\n', 'Total', utilityTotal)
  fprintf(ofid, '%-20s %10.2f\n', 'Total/patient',
          utilityTotal/nPatients, '#')

  # sum of costs tallied by state...
  fprintf(ofid, '\n')
  fprintf(ofid, 'Sum of costs tallied by state\n')
  fprintf(ofid, '-----------------------------\n')
  fprintf(ofid, '(state) %-25s %14s %5s\n', 'name',
                'total (#)', '(%)')
  for (s in 1:nStates) {
    pc = 100.0 * stateCost[s, 'total'] / costTotal
    fprintf(ofid, '(%5i) %-25s %14.2f %5.1f\n', s,
            state$name[s], stateCost[s, 'total'], pc)
  }

  # sum of costs tallied by state per patient...
  fprintf(ofid, '\n')
  fprintf(ofid, 'Sum of costs tallied by state (per patient)\n')
  fprintf(ofid, '-----------------------------\n')
  fprintf(ofid, '(state) %-25s %14s %5s\n', 'name',
          'total (#)', '(%)')
  for (s in 1:nStates) {
    pc = 100.0 * stateCost[s, 'total'] / costTotal
    fprintf(ofid, '(%5i) %-25s %14.2f %5.1f\n', s,
            state$name[s], stateCost[s, 'total']/nPatients, pc)
  }

  # sum of utilities tallied by state...
  fprintf(ofid, '\n')
  fprintf(ofid, 'Sum of utilities tallied by state\n')
  fprintf(ofid, '---------------------------------\n')
  fprintf(ofid, '(state) %-25s %13s %5s\n', 'name',
          'total', '(%)')
  for (s in 1:nStates) {
    pc = 100.0 * stateUtility[s, 'total'] / utilityTotal
    fprintf(ofid, '(%5i) %-25s %13.2f %5.1f\n', s,
            state$name[s], stateUtility[s, 'total'], pc)
  }

  # --------------------------------------------------------------------
  # write state information files
  # --------------------------------------------------------------------
  if (!is.na(stub)) {
    write.csv(popm, file=paste(stub, '-pop.csv', sep=""), row.names=F)
    write.csv(pspm, file=paste(stub, '-psp.csv', sep=""), row.names=F)
    write.csv(entm, file=paste(stub, '-ent.csv', sep=""), row.names=F)
    write.csv(utlm, file=paste(stub, '-utl.csv', sep=""), row.names=F)
  }

  # --------------------------------------------------------------------
  # save internal structures for continuation runs
  # --------------------------------------------------------------------
  if (!is.na(stub)) {
    toSave <- c('patient', 'cumEntries', 'cumOccupancies', 'stateCost',
                'stateUtility')
    save(list=toSave, file=paste(stub, '.Rdata', sep=""))
  }

  # --------------------------------------------------------------------
  # close log file
  # --------------------------------------------------------------------
  close(ofid)

  # ---------------------------------------------------------------------
  # set return value for function...
  # ---------------------------------------------------------------------
  #cpp = costTotal / nPatients
  return(list(popm, pspm, entm, utlm))
}

