;;forageMOD v02 by Ben Davies, University of Utah
;; modified by Emily Coco, Yale University
;;08 February 2024

extensions [
  csv
  table
  profiler
]

breed [ foragers forager ]
foragers-own [ moves move-tracker energy burn-prob age offspring interactions]
;;patch regeneration not instantiated
patches-own [ veg-type foraged? burnt? regenerating? who-burned times-human-burned time-to-last-burn max-veg-type save-veg-type]
links-own [ counter ]

globals [ file-path patch-burn-list available-forage forage-per-capita self-burn other-burn offspring-count energy-intake current-veg-regime current-max-veg ]

to setup
  clear-all

  ;Set file path for the experiment
  let stamp1 (random 9999) + 1
  set file-path (word "preliminary-results/experiment_" stamp1 "_" )
  set patch-burn-list []

  ;Tracking for burn benefit
  set self-burn 0
  set other-burn 0

  ;Set up initial distribution of max veg types on the landscape based on input parameter
  if veg-distribution = "random" [
    ask patches [
      ifelse veg-cycle-start = "productive" [ ;productive environment
        set max-veg-type one-of [3 5 7]
        set current-max-veg 7
      ][ ;unproductive environment
        set max-veg-type one-of [3 5]
        set current-max-veg 5
      ]
    ]
  ]

  if veg-distribution = "patchy" [
    ask patches [
      ifelse veg-cycle-start = "productive" [ ;productive environment
        set max-veg-type one-of [3 5 7]
        set current-max-veg 7
      ][ ;unproductive environment
        set max-veg-type one-of [3 5]
        set current-max-veg 5
      ]
    ]
    repeat 15 [
      ask patches [
        set max-veg-type [max-veg-type] of one-of neighbors4
      ]
    ]
  ]

 ;Set up all patches unburned, unforaged, at lowest productivity
  ask patches [
    set save-veg-type max-veg-type
    set veg-type 1
    set pcolor scale-color green veg-type 10 1
    set foraged? false
    set burnt? false
    set regenerating? false
    set who-burned nobody
  ]

  set current-veg-regime veg-cycle-start

  ;Create base population of 100 agents with randomly distributed ages and probability of burning
  ask n-of 100 patches [
    sprout-foragers 1 [
      set energy 1500
      set color (10 * (1 + random 14)) + 4
      set shape "person"
      set burn-prob -0.1 + random-float 0.2
      set age random 50
      set offspring 0
      set interactions 0
      set move-tracker 0
      set moves 0
    ]
  ]

  reset-ticks
end

to profile
  setup
  profiler:reset
  profiler:start
  repeat 200 [go]
  profiler:stop
  let _fname "report.txt"
  carefully [file-delete _fname] []
  file-open _fname
  file-print profiler:report
  file-close
end

to go

  if ticks >= tick-limit [
    if export? = true [
      set patch-burn-list lput patch-information patch-burn-list

      export-data
    ]
    stop
  ]

  ;Reset trackers
  set self-burn 0
  set other-burn 0
  if ticks mod (cycle-duration / 2) = 0 and ticks != 0 [
    set patch-burn-list lput patch-information patch-burn-list
  ]


  ;Alternate between productive-unproductive environments
  if ticks mod cycle-duration = 0 and ticks != 0 [
    ;see 'Cycle maximum productivity of different patches'
    cycle-veg
  ]

  ;see 'Update vegetation routine'
  ask patches [ update-veg ]

  ;Natural ignition
  ask n-of ((count patches) * natural-ignition) patches [
    set burnt? true
    set who-burned nobody
    set veg-type 0
  ]

  ;Update forage availability trackers
  set available-forage sum [ veg-type * veg-type-modifier ] of patches ;total forage
  set forage-per-capita  sum [ veg-type * veg-type-modifier ] of patches / count foragers ;per capita

  ;Reset forager movement and assess how many moves to make
  ask foragers [
    set moves 0
    set move-tracker 0
    assess-movement
  ]

  ;;after determining which agents need to move, accrue foraging cost
  ask foragers with [moves > 0] [
    set energy energy - forager-energy-requirement ;reduce energy
  ]

  ;foraging loop based on which agents still have moves to make
  while [ any? foragers with [ moves > 0] ] [
    ask foragers [
      check-interactions
    ]

    ;Agents forage and move
    ask foragers with [moves > 0] [
      ifelse (([ burnt? ] of patch-here = false) and ([ foraged? ] of patch-here = false) and ([ regenerating? ] of patch-here = false)) [
        ;output-print (word "agent " who " is foraging and moving")
        forage ;see 'Foraging routine'
        move
        ;set energy energy - movement-cost
      ][
        ;output-print (word "agent " who " is moving")
        move
        set energy energy - movement-cost
      ]
    ]
    ;output-print forager-list
  ]

  ;Agents check whether they are dead or reproducing, and age one time step
  ask foragers [
    if energy < 0 [ ;if agent doesn't meet energy, then die
      die
    ]
    if energy >= reproduction-threshold [ reproduce ] ;if agent exceeds double energy needs, reproduce (see 'Reproduction routine')
    set age age + 1 ;age
    if age = 100 [ die ]  ;optional lifespan limit
  ]

  if count foragers = 0 [ ;if all the agents are dead, stop the model
   if export? = true [
      export-data
    ]
    stop
  ]
  tick

end

;Foraging routine
to forage
  ;foraging behavior
  set energy energy + (veg-type * veg-type-modifier)

  ;check if space was previously burned
  if veg-type > 1 [
    ifelse who-burned = self [
      set self-burn self-burn + 1
    ][
      if who-burned != nobody [set other-burn other-burn + 1]
    ]
  ]

  ;set patch to foraged
  let vt [veg-type] of patch-here
  ask patch-here [
    set foraged? true
    set veg-type 0
  ]

  ;burning behavior
  if vt <= burn-veg-type-threshold [ ;;set a limit on what type of vegetation agents are willing to burn
    if foragers-burn? = true [
      if count ([neighbors] of patch-here) with [burnt? = true] <= burnt-neighbor-limit [ ;;preference for burning areas surrounding by green areas
        if (random-float 1.0) < burn-prob [
          set energy energy - burn-cost
          ask patch-here [
            set burnt? true
            set time-to-last-burn 0
            set times-human-burned (times-human-burned + 1)
            set pcolor black
            set who-burned myself
            set veg-type 0
          ]
        ;get a little extra energy from burning
          set energy energy + veg-type-modifier
        ]
      ]
    ]
  ]

end

; 'Cycle maximum productivity of different patches'
to cycle-veg
  ifelse current-veg-regime = "productive" [
    ask patches with [save-veg-type > 3] [
      set max-veg-type save-veg-type - 2
    ]
    set current-max-veg 5
    set current-veg-regime "unproductive"
  ][
    ask patches [
      set max-veg-type save-veg-type
    ]
    set current-max-veg 7
    set current-veg-regime "productive"
  ]

end

;'Update vegetation routine'
to update-veg
  set time-to-last-burn time-to-last-burn + 1

  ;Burnt patches are restored to highest productivity (after lag)
  ifelse burnt? = true [
;    if times-human-burned > regen-threshold [
;      set max-veg-type 5
;      set regenerating? true
;    ]

    set veg-type max-veg-type
    set burnt? false
    set foraged? false
  ]
  [
   ;Unburnt patches that were foraged are now restored to lowest productivity level
   ifelse veg-type > 1 [
      ifelse foraged? = true [
        set veg-type 1
        set foraged? false
      ]
      [
        ;Unburnt patches with productivity > 1 that were not foraged are reduced by one productivity
        set veg-type veg-type - 1
      ]
    ]
    [
      ;Unburnt patches with productivity = 1 remain at lowest productivity
      set veg-type 1
       set foraged? false
    ]
  ]

  let regen-threshold 10
  if regenerating? = true [
    if time-to-last-burn > regen-threshold * 2 [
      set regenerating? false
      set max-veg-type save-veg-type
    ]
  ]
   ;Update patch color
    set pcolor scale-color green veg-type 10 1

end

; 'Determine how many moves agent should make at each tick'
to assess-movement
  let required-moves 0
  let possible-total-energy 0

  ;agents check their local surroundings up to an input maximum number of moves
  let checks forager-moves

  let r 0.5

  ;test possible available energy at increasingly large radii to determine how far to travel to meet energy requirements
  while [checks > 0] [
    let previous-patches patches in-radius r
    set r r + 1
    ;agents make their assessment based on maximum available energy to forage
    let new-patches patches in-radius r with [ not member? self previous-patches]
    let available veg-type-modifier * (max ([veg-type] of (new-patches)))
    set possible-total-energy energy + available

    ;agents add an additional move until they have enough energy to reproduce and do their next round of foraging
    ifelse possible-total-energy < (reproduction-threshold + forager-energy-requirement) [
      set required-moves required-moves + 1
    ][
      stop ;do not add additional move if energy needs would be satisified by one fewer moves
    ]

    set checks checks - 1
  ]

  ;set limit on number of movements each forager can make
  set moves min (list forager-moves required-moves)
  set move-tracker moves

end


to move
  ;;movement
  if movement-model = "Directed Walk" [
    move-to max-one-of neighbors [ veg-type ]
  ]
  if movement-model = "Random Walk" [
    move-to one-of neighbors
  ]
  set moves moves - 1

end


to check-interactions
  let neighborhood neighbors

  ;increase the link age counter by 1
  ask my-links [
    let check false
    ask other-end [
     set check member? patch-here neighborhood
    ]

    ifelse check [
      set counter 0 ;if agent sees one of their links again, reset link age
    ][
      set counter counter + 1 ;increase link age of all links not currently on neighboring patches
    ]


    ;remove old links
    if counter > forager-moves * 5 [ ;currently links remain over ~5 move/forage sequences
      die
    ]
  ]

  ;create new links with foragers on neighboring patches
  let nforagers turtles-on neighbors
  create-links-with nforagers with [not link-neighbor? myself] [
    set counter 0
  ]

  set interactions count link-neighbors

  ask links [
    hide-link
  ]
end


to reproduce
  let bp burn-prob
  let mn moves
  let c color
  hatch 1 [
    set color c
    set energy forager-energy-requirement
    set age 0
    set offspring 0
    if foragers-burn? = true [
  ;  if bp > 0 and bp < 1 [
    ;set burn-prob bp + one-of [ 0.01 -0.01]
      ifelse random 2 = 0 [
        set burn-prob bp + 0.01
      ][
        set burn-prob bp - 0.01
      ]
  ;  ]
    ]
  ]
  set energy energy - reproduction-cost
  set offspring offspring + 1
end

;'Calculate global Moran's I for all patches based on veg-type'
to-report morans-I
  let m mean [[veg-type] of self] of patches

  let N count patches
  let all 8 * N

  let num sum [sum [([veg-type] of self - m) * ([veg-type] of myself - m)] of neighbors] of patches
  let denom sum [([veg-type] of self - m) ^ 2] of patches

  report (N / all) * (num / denom)
end

;'Calculate Simpson's diversity index of veg-types'
to-report simpsons-diversity
  let denom (count patches) * ((count patches) - 1)
  let num 0

  let vtypes remove-duplicates [veg-type] of patches
  foreach vtypes [ x ->
    let c count patches with [veg-type = x]
    set num num + (c * (c - 1))
  ]

  report num / denom
end

to-report patch-information
  let burn-list []
  ask patches [
    set burn-list lput (list ([pxcor] of self) ([pycor] of self) times-human-burned ticks) burn-list
  ]
  report burn-list
end

to export-data

  set patch-burn-list reduce sentence patch-burn-list
  file-open (word file-path "human-burning-amounts.csv")
  csv:to-file (word file-path "human-burning-amounts.csv") patch-burn-list
  file-close

  export-plot "Vegetation Type Proportions" (word file-path "vegetation-types.csv")
  export-plot "Burning Behavior" (word file-path "burning-behavior.csv")
  export-plot "Population" (word file-path "population.csv")
  export-plot "Self vs Other Benefit" (word file-path "benefit-distribution.csv")
  export-plot "Forager Interactions" (word file-path "forager-interactions.csv")
  export-plot "Forager Moves" (word file-path "forager-moves.csv")

end
@#$#@#$#@
GRAPHICS-WINDOW
211
123
627
540
-1
-1
8.0
1
10
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

BUTTON
212
16
275
49
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
17
13
181
46
natural-ignition
natural-ignition
0
0.01
0.005
0.001
1
NIL
HORIZONTAL

BUTTON
282
16
345
49
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
682
66
882
216
Population
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" "plot count foragers"

PLOT
902
391
1102
541
Burning Behavior
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS
"mean_burn_prob" 1.0 0 -16777216 true "" "ifelse foragers-burn? = true [\n  ifelse count foragers > 0 [ \n    plot mean [ burn-prob ] of foragers \n  ] \n  [ \n    plot 0 \n  ]\n ]\n [\n  plot 0  \n]\n  "
"high.CI_burn_prob" 1.0 0 -7500403 true "" "ifelse foragers-burn? = true [\n  ifelse count foragers > 2 [ \n    plot mean [ burn-prob ] of foragers + standard-deviation [ burn-prob ] of foragers \n  ] \n  [ \n    plot 0 \n  ]\n ]\n [\n  plot 0  \n]\n  "
"low.CI_burn-prob" 1.0 0 -7500403 true "" "ifelse foragers-burn? = true [\n  ifelse count foragers > 2 [ \n    plot mean [ burn-prob ] of foragers - standard-deviation [ burn-prob ] of foragers \n  ] \n  [ \n    plot 0 \n  ]\n ]\n [\n  plot 0  \n]\n  "

SWITCH
18
93
157
126
foragers-burn?
foragers-burn?
0
1
-1000

PLOT
1119
229
1319
379
Average Energy Intake
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -6917194 true "" "ifelse count foragers > 0 [ plot mean [ energy ] of foragers ] [ plot 0 ]"
"pen-1" 1.0 0 -1712915 true "" "ifelse count foragers > 2 [ plot mean [ energy ] of foragers + standard-deviation [ energy ] of foragers  ] [ plot 0 ]"
"pen-2" 1.0 0 -1712915 true "" "ifelse count foragers > 2 [ plot mean [ energy ] of foragers - standard-deviation [ energy ] of foragers  ] [ plot 0 ]"

PLOT
902
67
1102
217
Age Structure
NIL
NIL
0.0
120.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ age ] of foragers"

PLOT
901
228
1101
378
Available Forage Per Capita
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pc" 1.0 0 -10899396 true "" "plot forage-per-capita"

PLOT
1119
391
1319
541
Self vs Other Benefit
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Self" 1.0 0 -955883 true "" "plot self-burn"
"Other" 1.0 0 -13345367 true "" "plot other-burn"

CHOOSER
17
264
155
309
movement-model
movement-model
"Directed Walk" "Random Walk"
1

SLIDER
17
226
189
259
forager-moves
forager-moves
5
15
10.0
1
1
NIL
HORIZONTAL

SLIDER
12
351
195
384
forager-energy-requirement
forager-energy-requirement
500
3000
1500.0
500
1
NIL
HORIZONTAL

SLIDER
11
388
195
421
reproduction-threshold
reproduction-threshold
forager-energy-requirement
5000
3000.0
500
1
NIL
HORIZONTAL

SLIDER
11
427
196
460
reproduction-cost
reproduction-cost
500
reproduction-threshold
500.0
500
1
NIL
HORIZONTAL

SLIDER
17
51
181
84
veg-type-modifier
veg-type-modifier
50
500
200.0
50
1
NIL
HORIZONTAL

SLIDER
11
465
196
498
movement-cost
movement-cost
0
500
100.0
100
1
NIL
HORIZONTAL

BUTTON
356
16
441
49
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
451
81
605
114
cycle-duration
cycle-duration
0
2000
100.0
100
1
ticks
HORIZONTAL

CHOOSER
207
70
317
115
veg-cycle-start
veg-cycle-start
"productive" "unproductive"
0

PLOT
683
228
883
378
Vegetation Type Proportions
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"veg7" 1.0 0 -15575016 false "" "plot (count patches with [veg-type = 7]) / (count patches)"
"veg6" 1.0 0 -15040220 false "" "plot (count patches with [veg-type = 6]) / (count patches)"
"veg5" 1.0 0 -14439633 true "" "plot (count patches with [veg-type = 5]) / (count patches)"
"veg4" 1.0 0 -13840069 true "" "plot (count patches with [veg-type = 4]) / (count patches)"
"veg3" 1.0 0 -11085214 true "" "plot (count patches with [veg-type = 3]) / (count patches)"
"veg2" 1.0 0 -8330359 true "" "plot (count patches with [veg-type = 2]) / (count patches)"
"veg1" 1.0 0 -5509967 true "" "plot (count patches with [veg-type = 1]) / (count patches)"
"veg0" 1.0 0 -16777216 true "" "plot (count patches with [burnt? = true]) / (count patches)"

SLIDER
17
131
181
164
burnt-neighbor-limit
burnt-neighbor-limit
0
8
2.0
1
1
NIL
HORIZONTAL

INPUTBOX
467
10
543
70
tick-limit
200.0
1
0
Number

PLOT
684
391
884
541
Forager Moves
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "ifelse count foragers > 0 [ plot (mean [ move-tracker ] of foragers) \n] [ plot 0 ]\n"
"pen-1" 1.0 0 -7500403 true "" "ifelse count foragers > 2 [ plot mean [ move-tracker ] of foragers + standard-deviation [ move-tracker ] of foragers  ] [ plot 0 ]"
"pen-2" 1.0 0 -7500403 true "" "ifelse count foragers > 2 [ plot mean [ move-tracker ] of foragers - standard-deviation [ move-tracker ] of foragers  ] [ plot 0 ]"

SWITCH
557
18
661
51
export?
export?
0
1
-1000

SLIDER
10
504
196
537
burn-cost
burn-cost
0
500
0.0
100
1
NIL
HORIZONTAL

CHOOSER
323
70
433
115
veg-distribution
veg-distribution
"random" "patchy"
0

PLOT
1118
67
1318
217
Forager Interactions
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -7858858 true "" "ifelse count foragers > 0 [ plot mean [ interactions ] of foragers ] [ plot 0 ]"
"pen-1" 1.0 0 -1664597 true "" "ifelse count foragers > 2 [ plot mean [ interactions ] of foragers + standard-deviation [ interactions ] of foragers  ] [ plot 0 ]"
"pen-2" 1.0 0 -1664597 true "" "ifelse count foragers > 2 [ plot mean [ interactions ] of foragers - standard-deviation [ interactions ] of foragers  ] [ plot 0 ]"

BUTTON
730
23
822
56
NIL
cycle-veg
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
17
171
194
204
burn-veg-type-threshold
burn-veg-type-threshold
1
7
4.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

ForageMod is a simple model of foraging in an environment in which a disturbance (either natural or forager-initiated; e.g. fire) can temporarily improve local environmental productivity. 

## HOW IT WORKS

In the model, agents obtain resources (energy) from their environment, moving to new locations once local resources are exhausted. If an agent obtains more resources than needed to exceed a threshold, the agent can reproduce, adding a new agent to the world. If an agent fails to obtain enough resource to survive, the agent dies.

The energy obtained from different parts of the environmentis controlled by the *veg-type*, where a higher value provides more resources, while a lower value (baseline 1) provides less. A value of 0 indicates the patch has either previously been foraged or burned. Each patch is assigned a *max-veg-type* that it can achieve; this is determined by whether the model is in a productive vegetation regime (*curret-veg-regime*) or an unproductive one. During a productive state, *max-veg-types* are either 3, 5, or 7. During an unproductive state, *max-veg-types* are limited to 3 and 5, reducing the total possible resources available in the environment. These vegetation regimes cycle throughout the model as determined by the *cycle-duration* parameter.

Each time step, the environment updates in the following ways:
-Burned patches are reset to their *max-veg-type*
-Foraged patches that are not burned return to the minimum veg-type (1)
-Patches with a veg-type higher than 1 reduce their veg-type by 1

Following this, a random subset of patches are burned through "natural ignition" (e.g. lightning), setting their veg-type to 0. 

Movement occurs over a number of steps, and can occur in one of two ways:
-"Directed Walk", where agents move to the neighboring patch with the highest resource availability.
-"Random Walk", where agents move to any neighboring patch.

Agents assess how many steps they will take during a given tick by iteratively checking what is the maximum amount of energy they could obtain within increasingly larger radii. Agents add additional moves to their agenda until they determine they will have enough energy to reproduce and do their next round of foraging. The agents then carry out those steps and forage at each step when possible. For every step, the order of foragers is randomized.

When agents forage, they reduce the veg-type to 0. If an agent arrives on a patch that has a 0 value, there is an agent-specific probability (burn-prob) that the agent will burn the patch. At the start of the model, these probabilities are distributed evenly in the population between -0.1 and 0.1, with negative values resulting in no burning at all.

When an agent burns a patch, there is an associated burn cost, but they also receive a little extra energy.
 
Following reproduction, the new agent begins with enough resource to survive to the next time step. The new agent's *burn-prob* is the value of the parent +/- 0.01, making the new agent more or less likely to burn unproductive land than their parent.


## HOW TO USE IT

### Controls

#### *TODO: update to include new parameters*

To run the model, press Setup to initiate the simulation, then Go to set it running. The model begins with 100 agents in a landscape where all patches are veg-type 1. 


The percentage of patches that are burned naturally each time step is controlled by the natural-ignition slider. For example, if the slider is set to 0.05, then a random set of patches equivalent to 5% of all patches ignites naturally each time step. 

The amount of energy available in a patch of a given veg-type is controlled by the veg-type-modifier slider. For example, if the slider is set to 200, then a patch with a veg-type of 3 provides 600 energy units to a forager.

The capacity of the agents to burn can be turned on or off with the foragers-burn? switch. If this is turned off, foragers will not engage in any burning behavior.

The number of moves an agent takes each time step can be controlled by the forager-moves slider. For example, if the slider is set to 5, the agent performs 5 foraging moves per time step.

The type of movement used by the agents can be controlled by the movement-model chooser. These models are described in the preceding section.

The amount of energy required by each agent for survival can be controlled by the forager-energy-requirement slider. For example, if the slider is set to 1000, an agent requires a net 1000 energy units at the end of its turn or else it dies. Note: new agents in the model begin with the number of energy units specified by this slider.

The amount of energy required by an agent to reproduce during its turn is controlled by the reproduction-threshold slider. For example, if the slider is set to 2000, an agent must obtain at least 2000 energy units in order to reproduce. The minimum value of this slider is equal to the value of the forager-energy-requirement slider.

The amount of energy expended by a reproducing agent is controlled by the reproduction-cost slider. For example, if the slider is set to 500, a reproducting agent expends 500 energy units to reproduce.

The amount of energy expended by an agent moving between patches is controlled by the movement-cost slider. For example, if the slider is set to 100, an agent expends 100 energy units for each move it makes. 

### Outputs

#### Plots

Population: the total number of agents at the end of each time step

Age structure: the distribution of ages for each agent

Forager interactions: the average number of other agents each agent has recently encountered (with 1 standard deviation)

Vegetation type proportions: the proportional number of patches with each *veg-type* value

Available Forage Per Capita: total available energy at the world evenly divided among agents prior to foraging

Average Energy Intake: the average amount of energy taken in by an agent at the end of each time step (with 1 standard deviation)

Forager moves: the average number of steps agents take at each tick (with 1 standard deviation)

Burning behavior: the average burn-prob in the population of agents (with 1 standard deviation)

Self vs Other Benefit: tracks the number of patches where the energy gained from them was gained by the last agent to burn it (self; orange) or someone else (other; blue)

#### Other outputs
A file that records the frequency of human burning events for each patch (x, y coords)


## THINGS TO NOTICE

-If foragers cannot burn, the natural ignitions will control carrying capacity of the model. For random walk movement settings, this will likely result in populations dying out from increased likelihood of agents passing over patches that have already been foraged.

-Under all configurations of this model, the benefits that come from burning are overwhelmingly transferred to other agents rather than the agent doing the burning.

-If agents use either of the "walk" movement schemes, the local benefits of agent burning result in greater selection for burning behavior; that is, agents moving locally are more likely to benefit from their own burning, and will therefore agents doing more burning will be more likely to reach the reproduction threshold, potentially reaching higher levels of burn-prob. This process will feedback, producing higher populations and more prevalent burning behavior in the population. 

-If movements incur costs, then a "random walk" movement will likely result in many agents initially dying as agents frequently retrace their steps over foraged patches. Alternatively, a "directed walk" will result in slow growth as agents avoid senesenced patches that require burning. 

-If agents use the "jump" movements, then the stochastic effects of movement mean agents rarely obtain resources from patches they burn, giving no benefit to burners; however, under some circumstances, the random effects of burning behavior inheritance of can result in feedbacks for productivity, producing peaks in population. 


## EXTENDING THE MODEL

-Currently, the model does not account for forager age or experience; therefore, agents can die from lack of resources irrespective of their age. It would be useful to connect success with longevity.

-There is a high disparity in the amount of resources agents obtain each time step, with some agents substantially exceeding the reproduction threshold. It would be interesting to add a component that allows agents to share resources.


## CREDITS AND REFERENCES

Ben Davies, University of Utah, 2020
Emily Coco, Yale University, 2024

## VERSION HISTORY

vb01 base model 1 Dec 2020
vb02 replaced hard-coded forager variables with sliders 16 Dec 2020
v01 Updated burning activities to include recently foraged patches, renamed some variables
Web: b-davies.github.io/files/ForageModv01.html
v02 Updated environmental conditions, burning limits, and foragers do movement assessment, added some new outputs 2024
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test-burn-neighbor-limits_prod-start" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tick-limit">
      <value value="2000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cycle-duration">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natual-ignition">
      <value value="0.005"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="veg-cycle-start">
      <value value="&quot;productive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="veg-distribution">
      <value value="&quot;random&quot;"/>
      <value value="&quot;patchy&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foragers-burn?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="burnt-neighbor-limit">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-model">
      <value value="&quot;Random Walk&quot;"/>
      <value value="&quot;Directed Walk&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forager-moves">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forager-energy-requirement">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="burn-cost">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="veg-type-modifier">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="movement-cost">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-threshold">
      <value value="3000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reproduction-cost">
      <value value="500"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
