breed [ services service ]
breed [ needs need ]
breed [ clusters cluster]
;breed [ links link ]

;my-FCs: Vector which contains a number (0-1) that depicts an service's performance with a specific feature.
;           *FCs are enumerated.
;W*: list of values from other services
;S*: list of coalitions from other services
;joined: the coalition the service is associated with
services-own [ my-FCs W S joined my-price]

;requirements: Vector of requirements for a need that identifies the need of each feature.
;              *Size determined by num-FCs and enumerated in the same manner as my-FCs.
;coalition-in-use: As coalitions are associated with a need, they are added to this list for record-keeping
needs-own [ requirements coalition-in-use ]


clusters-own [id beta c-FCs LServices LClusters LBundles]

;L: global list of coalitions
globals [ L num-clusters RedPos BluePos GreenPos YellowPos OrangePos bg-color fg-color]

patches-own [ mem ]

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set RedPos -20
  set BluePos -15
  set GreenPos 5
  set YellowPos -5
  set OrangePos 15
  set L []

  set bg-color white ; <-------- SET BACKGROUND COLOR --------
  set fg-color bg-color - 4 ; <-------- SET FOREGROUND COLOR --------
  
  ask patches [ set pcolor bg-color ]
  
  ; Generate Clusters
  set num-clusters (2 ^ num-FCs) - 1
  create-clusters num-clusters
  setup-clusters
    
  create-services num-services
  create-needs num-needs
  setup-services
  setup-needs
end


to setup-services
  output-show "my-FCs:"
  ask services 
  [ 
    ;set my-FCs build-list-randomly num-FCs 1
    ;set my-FCs build-list-randomly num-FCs 2
    set my-FCs build-list-service num-FCs
    set W []
    set S []
    set label who
    set joined []
    set shape "box"
    set size 1
    set color red
    ;Carlos:
    set my-price random 500
   set label-color black
    ;setxy -25 random-ycor
    set xcor -25
    set ycor (30 / num-services) * ceiling((who - num-clusters) / 2) * (-1)^((who - num-clusters))
    
    create_my_links self my-FCs
    
    output-show (list my-FCs "Price:" my-price)
    
    ;Add himself to L
    set L lput (list who) L 
  ]
end

to setup-needs
  output-show "required FCs:"
  ask needs [
    set coalition-in-use []
    set label coalition-in-use
    ;set requirements build-list-randomly num-FCs 2
    set requirements build-list-service num-FCs
    
    set shape "person"
    set size 3
    set color blue ;(color - 20) ;make it darker for readability
    ;set xy 25 random-ycor
    set xcor 25
    ;let YPos (30 / num-needs) * ceiling((who - num-needs) / 2) * (-1)^(who)
    let YPos1 (30 / num-needs) 
    let YPos2 ceiling(((who - (num-services + num-clusters)) - num-needs) / 2) 
    let Ypos3 (-1)^((who - (num-services + num-clusters)))
    let Ypos (YPos1 * YPos2 * YPos3)
    set ycor YPos
    set label-color black
    ]
  
    ; We assume that there is always a need requiering all the FCs
    let ThisNeed random num-needs 
    set ThisNeed ThisNeed + (num-clusters + num-services)
    show " ALL-FEATURE NEED"
    show ThisNeed
    ask need ThisNeed
    [
      set requirements build-list-need num-FCs
      show requirements
    ]
    
    ask needs
    [
      set label requirements
      output-show requirements
    ]
    
end

to setup-clusters
  ;output-show "clusters:"
  ask clusters [
    set LServices []
    set LClusters []
    set LBundles []
    set c-FCs []
    set id who + 1;- (num-needs + num-services) + 1
    set label id
    set hidden? true
    set shape "circle"
    set size 1
    ;setxy RedPos random-ycor
    set xcor RedPos
    set ycor (30 / num-clusters) * ceiling(who / 2) * (-1)^(who)
    ;output-show id
    set label-color black
    ]
end

to setup-L
  ; Compute L
  ; Expand All the Clusters
  ask clusters
  [
    let MyBundles LBundles
    foreach MyBundles
    [
      let MyB ?
      set L lput MyB L
    ]
  ]
  
  set L remove-duplicates L
  
end



to-report add-to-list [ i lst ]
  report lput i lst
end

to-report build-list-randomly [ sz limit ]
  ;lst: The list returned.
  let lst 0
  
  set lst []
  ifelse (limit <= 1) [ repeat sz [ set lst lput (((random (limit * 100 - 1)) + 1) / 100) lst ] ] ;need to normalize if limit is less than 1
  [ repeat sz [ set lst lput (random limit) lst ] ]
  report lst
end


to-report build-list-need [ sz ]
  ;lst: The list returned.
  let lst 0
  
  set lst []
  repeat sz [ set lst lput (1) lst ]
  report lst
end

to-report build-list-service [ sz ]
  ;lst: The list returned.
  let lst 0
  let NumOnes 0
  let NumZeros 0
  set lst []
  repeat sz 
  [ 
    let MyFC random 2
    ifelse(MyFC = 1)
    [
      set NumOnes (NumOnes + 1)
    ]
    [
      set NumZeros (NumZeros + 1)
    ]
    
    if (NumOnes = sz)
    [
      set MyFC 0
    ]
    
    if(NumZeros = sz)
    [
      set MyFC 1
    ]
    
    set lst lput (MyFC) lst 
  ]
  
  
  
  report lst
end


to create_my_links [YO these-FCs]
  let current_FCs 0 ;[my-FCs] of (service ?)
  let sum_value 0
  let index 0
      
  ask YO
  [
    let ThisID who
    set current_FCs these-FCs
    set sum_value 0
    set index 0
    show current_FCs   
    repeat num-FCs
    [
       if( (item index current_FCs) > 0)
       [ set sum_value (sum_value + (2 ^ index))]
       set index ( index + 1 ) 
    ]
    show sum_value
    if (sum_value > 0)
    [
      create-link-to cluster (sum_value - 1)
      [
         set color red
      ]
      ask cluster (sum_value - 1)
      [
        ;let MyC count my-in-links
        set hidden? false
        set color red; random 255 
        set c-FCs current_FCs
        set label c-FCs
        face YO
        set LServices lput ThisID LServices
        set LServices remove-duplicates LServices
      ]
    ]
  ]
end

to combine-lower-clusters
  ; Code here
  show " combine-lower-clusters "
  show L
  let Objective num-clusters
  ;show Objective
  ; Combine Lower Clusters
  let MySTOP ceiling(Objective / 2)
  ;show MySTOP
  let P1 0
  let Current P1
  let Previous 0
  let HMany MySTOP - P1
  ;show P1
  
  ; New stuff to stop exploration
  let Count1 0
  let MySTOPCL 2
  
  repeat HMany - 1
  [
    ;show L
    show " Exploring "
    show P1
    if (Current != P1)
    [
      set Previous P1 - 1 ;Current
      set Current P1
      ;show Previous
      
      let i 0
      let CI Previous + 1
      repeat CI
      [
        show i
        if(0 = (overlapped i P1))
        [
          let CiP1 (i + P1) + 1
 
          let LinkC random 255
          
          ask cluster i
          [
            create-link-to cluster P1
            [
              set color LinkC
              ;set width 3
            ]            

            create-link-from cluster P1
            [
              set color LinkC
              ;set width 3
            ]            
            
            create-link-to cluster CiP1
            [
              set color LinkC
            ]
            
            set hidden? false
            set color blue
            set size 2
            ;setxy BluePos random-ycor
            set xcor BluePos
            ;set ycor (30 / num-clusters) * ceiling(who / 2) * (-1)^(who)
            face cluster P1
          ]
           
          ask cluster P1
          [
            create-link-to cluster CiP1
            [
              set color LinkC
            ]
            set hidden? false
            set color blue          
            set size 2            
            ;setxy BluePos random-ycor
            set xcor BluePos
            ;set ycor (30 / num-clusters) * ceiling(who / 2) * (-1)^(who)
            face cluster i
          ]
          
          ask cluster CiP1
          [
            ;create-link-with cluster CiP1
            set hidden? false
            set color yellow      
            set size 2            
            ;setxy YellowPos random-ycor            
            set xcor YellowPos
            ;set ycor (30 / num-clusters) * ceiling(who / 2) * (-1)^(who)
            let MyC []
            set MyC lput i MyC
            set MyC lput P1 MyC
            set LClusters lput MyC LClusters
            set LClusters remove-duplicates LClusters
            set c-FCs combine-FCs i P1
            set label c-FCs
          ]
        
          ; Counting a new cluster
          set Count1 (Count1 + 1)
          
        ]; END - if(0 = (overlapped i P1))
        ;show i
        set i (i + 1)
      ]
    ]
    set P1 (P1 + 1)
    
    ; Analyse whether we reached MySTOP
    if (Count1 = MySTOPCL)
    [
      show " MySTOPCL reached "
      show Count1
      stop
    ]
  ]  
end

to compute-coalitions
  show L
  let Objective num-clusters
  let MySTOP ceiling(Objective / 2)
  ;show MySTOP
  let P1 0
  let Current P1
  let Previous 0
  let HMany MySTOP - P1
  
  ;
  ; Generate Solution Clusters
  ;  
  
  show "Combining Upper and Lower"
  
  let MySTOPCL 1
  let Count2 0
  
  let MyPU (Objective - 1)
  while [MyPU >= MyStop]
  [
    show MyPU    
    let A[]
    let B[]
    let ThisUP nobody
    ask cluster MyPU
    [
      set ThisUP self 
      set A c-FCs
      ifelse (empty? A)
      [
        set A []
        ; error
      ]
      [
        let PL 0
        let ThisLO nobody
        let HManyPL MyStop
        repeat HManyPL
        [
          show PL
          
            ask cluster PL
            [
              set B c-FCs
              set ThisLO self
              ifelse (empty? B)
              [
                set B []
                ; cannot be combined
              ]
              [
                if(0 = (overlapped MyPU PL))
                [ 
                  ; Lower
                  create-link-to cluster MyPU
                  [
                    set color yellow
                  ]
                  create-link-from cluster MyPU
                  [
                    set color yellow
                  ]                  
                  set color green
                  set xcor GreenPos
                  
                  ask cluster MyPU
                  [
                    set color green
                    set size 2
                    set xcor GreenPos
                  ]
                  
                  ; Combining upper with lower in MyR
                  let MyR MyPU + PL
                  set MyR (MyR + 1)
                  ask cluster MyR
                  [
                    set color orange 
                    set size 2
                    set hidden? false
                    ;setxy OrangePos random-ycor
                    set xcor OrangePos 
                    
                    create-link-from ThisLO
                    [
                      set color yellow
                    ]                     
                    create-link-from ThisUP
                    [
                      set color yellow
                    ]                                         
                    let MyC []
                    set MyC lput PL MyC
                    set MyC lput MyPU MyC
                    set LClusters lput MyC LClusters
                    set LClusters remove-duplicates LClusters
                    set c-FCs combine-FCs PL MyPU
                    set label c-FCs
                  ]
                  
                  set Count2 (Count2 + 1)
                  
                ]; END - if(0 = (overlapped MyPU PL))
              ]
            ]
          set PL (PL + 1)
        ]
      ]
    ] 
    set MyPU (MyPU - 1)
    
    ; Have we reached MySTOPCL
    if(Count2 = MySTOPCL)
    [
      show " MySTOPCL 2 has been reached - stopping exploration "
      show Count2
      stop
    ]
    
  ]; END - while [MyPU >= MyStop]
  
  setup-L
  
end

to-report overlapped [Cluster-i Cluster-P1]
  let A []
  let B []
  let MyRes 1
  
  ask cluster (Cluster-i)
  [
    show c-FCs
    set A c-FCs
  ]
  
  ask cluster (Cluster-P1)
  [
    show c-FCs
    set B c-FCs
  ]
  
  ifelse( (empty? A) or (empty? B) )
  [
    set MyRes 1
  ]
  [
    let OP (map [?1 + ?2 = 2] A B)
    show OP
    ifelse (member? true OP)
    [
      set MyRes 1
    ]
    [
      set MyRes 0
    ]
  ]
  report MyRes
end


to-report combine-FCs [Cluster-i Cluster-P1]
  let MyC (Cluster-i + Cluster-P1) + 1
  let A []
  let B []
  let CF []
  let MyRes 1
  
  let S1 []
  let S2 []
  let B1 []
  let B2 []
  
  ask cluster (Cluster-i)
  [
    show c-FCs
    set A c-FCs
    set S1 LServices
    set B1 LBundles
  ]
  
  ask cluster (Cluster-P1)
  [
    show c-FCs
    set B c-FCs
    set S2 LServices
    set B2 LBundles
  ]
  
  ; Combining services with services
  let MyBundle []  
  foreach S1
  [
    let MyS1 ?
    foreach S2
    [
      set MyBundle []
      let MyS2 ?
      set MyBundle lput MyS1 MyBundle
      set MyBundle lput MyS2 MyBundle
      set MyBundle sort MyBundle
      
      ;set MyBundles lput MyBundle MyBundles
      ask cluster MyC
      [
        set LBundles lput MyBundle LBundles
      ]
    ]
  ]
  
  ; Combining S1 with B2
  foreach S1
  [
    let MyS1 ?
    foreach B2
    [
      ;set MyBundle []
      let MyB2 ?
      set MyB2 lput MyS1 MyB2
      set MyB2 sort MyB2
      ask cluster MyC
      [
        set LBundles lput MyB2 LBundles
      ]
    ]
  ]
  
  ; Combining S2 with B1
  foreach S2
  [
    let MyS2 ?
    foreach B1
    [
      ;set MyBundle []
      let MyB1 ?
      set MyB1 lput MyS2 MyB1
      set MyB1 sort MyB1
      ask cluster MyC
      [
        set LBundles lput MyB1 LBundles
      ]
    ]
  ]
  
  ; Combining B1 with B2
  foreach B1
  [
    let MyB1 ?
    foreach B2
    [
      let MyB2 ?
      
      ; add element by element the services in MyB1
      foreach MyB1
      [
        let MySubB1 ?
        set MyB2 lput MySubB1 MyB2
      ]
      set MyB2 sort MyB2
      ask cluster MyC
      [
        set LBundles lput MyB2 LBundles
      ]
    ]
  ]
  
  set LBundles remove-duplicates LBundles
  
  set CF (map [?1 + ?2] A B)
  show CF
  report CF
end

;
;
; FIND ANSWER TO OUR NEED
;

to find-co
 
  ;foreach ([who] of needs) [ ask services [ find-coalition who ? ] ]    
  
  ask needs
  [ 
    set coalition-in-use []
    let ThisNeed self
    let SolBundles []
    let N_FCs requirements
    ;ask cluster ((2 ^ num-FCs) - 2)
    ;[
    ;  set SolBundles lbundles 
    ;]
    
    ask clusters
    [
      let ThisCluster who
      let C_FCs c-FCs
      if(N_FCs = C_FCs)
      [
        ifelse(empty? lbundles)
        [
          ifelse(empty? lservices)
          [
          ]
          [
            ;set SolBundles lservices
            ; Create a list for each service
            foreach lservices
            [
              let ThisSer ?
              let NewL []
              set NewL lput ThisSer NewL
              set SolBundles lput NewL SolBundles
            ]
            create-link-to ThisNeed
            [
              set color Blue
            ]
            face ThisNeed
            set Size 3
            forward 5
          ]
        ]
        [
          set SolBundles lbundles
          create-link-to ThisNeed
          [
            set color Blue
          ]
          face ThisNeed
          set size 3
          forward 5
        ]
      ]
    ]
    
    set coalition-in-use SolBundles
    
    ifelse(empty? coalition-in-use)
    [
      set color red
    ]
    [
      ;output-show coalition-in-use
      display-coalitions who
    ]
  ]    
  
end

to display-coalitions [ThisN]
    output-show "Best bundles (based on price):"  
    
    ask need ThisN
    [  
      ;output-show (list my-FCs "Price:" my-price)
      
      let Coalitions-Prices []
      let SumPrices 0
      let ThisC-Price []
      
      let TempL1 []
      let TempL2 []
      
      foreach coalition-in-use
      [
        set ThisC-Price []
        set SumPrices 0
        let ServicesinC ?
        
        
        
        foreach ServicesinC
        [
          let ThisService ?
          ask service ThisService
          [
            set SumPrices (my-price + SumPrices)
          ]
        ]
        
        set ThisC-Price lput SumPrices ThisC-Price  
        set ThisC-Price lput ServicesinC ThisC-Price
        set TempL1 []
        set TempL2 []
        
        ;output-show "ThisC-Price : "
        ;output-show ThisC-Price
        
        ifelse (empty? Coalitions-Prices)
        [
          set TempL1 lput ThisC-Price TempL1
          ;output-show "First TempL1 : "
          ;output-show TempL1
                  
        ]
        [
          ; insert ThisC-Price in the right position
          ; it can be inserted at the beginning, somewhere in the middle or at the end.
          let Ban 0
          
          foreach Coalitions-Prices
          [
            let ThisCoalition ?
            let ThisValue item 0 ThisCoalition
            ;output-show "Comparing : " 
            ;output-show SumPrices
            ;output-show ThisValue
            ; Inserting the new coalition at the beginning
            ifelse( (SumPrices < ThisValue) and (Ban = 0) )
            [
              set TempL1 lput ThisC-Price TempL1
              set TempL2 lput ThisCoalition TempL2
              set Ban 1
            ]
            [
              ifelse(Ban = 1) ; ThisC-Price has been inserted already
              [
                set TempL2 lput ThisCoalition TempL2
              ]
              [
                set TempL1 lput ThisCoalition TempL1
              ]
            ]
            
            ; 
          ]
          
          ; Inserting ThisC-Price at the end
          if (Ban = 0)
          [
            set TempL2 lput ThisC-Price TempL2
          ]
        ]
        ;output-show (list ServicesinC "Price:" SumPrices)
        
        ;output-show "TempL1 : "
        ;output-show TempL1
        ;output-show "TempL2 : "
        ;output-show TempL2
        ifelse(empty? TempL2)
        []
        [
          foreach TempL2
          [
            let ThisL2 ? 
            set TempL1 lput ThisL2 TempL1
          ]
        ]
        set Coalitions-Prices TempL1
        ;output-show Coalitions-Prices
      ]
      
      
      foreach Coalitions-Prices
      [
        let MyCoalition ?
        let MyPriceC item 0 MyCoalition
        let MyServices item 1 MyCoalition
        output-show (list MyServices "Price:" MyPriceC)
      ]
    ]    
end

;find-coalition algorithm.
to find-coalition [ service-i need_i ]
  ;L_i: list of coalitions containing service-i
  ;Max_i: [S_i* w_i*] retrieved after evaluating all coalitions in L_i
  ;w_max: the maximum value in W*
  ;S_max: the coalition corrisponding to w_max (W*)
  let L_i 0
  let Max_i 0
  let w_max 0
  let S_max 0
  
  
  set L_i coalitions service-i
  
  ; Remove coalitions that do not match needs
  set L_i coalitions-matching L_i service-i need_i
  
  show L_i 
  ;output-show (list " L_i : " L_i)
  
  ask need need_i
  [
    
    foreach L_i
    [
      let ThisC ?
      ;set color random 255
      set coalition-in-use lput ThisC coalition-in-use
      ;set label coalition-in-use
    ]
    
    set coalition-in-use remove-duplicates coalition-in-use
  ]
  
  
  
end

;Fetches the coalitions that contain member i
to-report coalitions [ i ]
  ;lst: the list returned
  let lst 0
  
  set lst []
  foreach L [ if( member? i ? ) [ set lst lput ? lst ] ]
  report lst 
end

;Keep coalitions matching the j need
to-report coalitions-matching [ MyCoalitions i j]
  ;lst: the list returned
  let lst 0
  show " Filtering Service"
  show i
  show MyCoalitions
  
  set lst []
  
  ;foreach L [ if( member? i ? ) [ set lst lput ? lst ] ]
  foreach MyCoalitions
  [
    let ThisCoalition ?
    let CombinedFCs []
    
    show ThisCoalition
        
    repeat num-FCs
    [
      set CombinedFCs lput 0 CombinedFCs
    ]
    
    ;show " Elements "
    foreach ThisCoalition
    [
      let ThisService ?
      ;let ThisFCs []
      ;show ThisService
      ask service ThisService
      [
         set CombinedFCs (map [?1 + ?2] CombinedFCs my-FCs)
      ]
    ]
    
    let ThisNeed []
    ask need j
    [
      set ThisNeed requirements
    ]
    
    let Res (map [?1 = ?2] CombinedFCs ThisNeed)
    
    show " Results "
    show Res
    
    ifelse (member? false Res)
    []
    [
      show " Valid coalition "
      set lst lput ThisCoalition lst
    ]
    
  ]
  
  report lst 
end

;Fetches the coalition with the highest value in a given set
;c_lst: list of possible coalitions
;need_i: current need
to-report max-coalition [ c_lst need_i ]
  ;maximum-value: v(S*)
  ;maximum-col: S*
  ;sum_vector: temporary space to hold the sum of my-FCs for each service in the coalition
  ;index: for moving through the elements in the my-FCs vector
  let maximum-value 0
  let maximum-col 0
  let sum_vector 0
  let sum_prices 0 
  let current_skills 0
  let current_price 0
  let index 0
   

  set maximum-value -1000 ;preset low

  foreach c_lst [  ;iterate through each coalition
    set sum_vector []
    set sum_prices 0
    repeat num-FCs [ set sum_vector lput 0 sum_vector ] ;reset summation vector

    ;fetch the my-FCs vector from each service
    foreach ? [
      set index 0
      set current_skills [my-FCs] of (service ?)
      set current_price [my-price] of (service ?)
      show "---Current Price"
      show current_price
      ;sum up prices
      set sum_prices (sum_prices + current_price)
      repeat num-FCs [ ;sum up the values in the my-FCs vectors for each service in the current coalition
        set sum_vector replace-item index sum_vector precision ( (item index sum_vector) + (item index current_skills) ) 2
        set index ( index + 1 ) ] ]
      set index 0
            
      show "-------SumPrices:"
      show sum_prices
      
      ;add random extra to sum_prices
      set sum_prices (sum_prices + random 50)
      show (list "Now sum_prices = " sum_prices)

    ;calculate the value for the coalition
    repeat num-FCs [
      set sum_vector replace-item index sum_vector (-1 * abs( precision ( (item index [requirements] of (need need_i)) - (item index sum_vector) ) 2 ))
      set index ( index + 1 ) ]
  
    
    ;if ( ((sum sum_vector) / (length ?)) >= maximum-value ) [  ;check v(S)/|S| against current maximum
    ;  set maximum-value precision ((sum sum_vector) / (length ?)) 2
    ;  set maximum-col ? ] ] 
  
    if ( ((sum_prices) / (length ?)) >= maximum-value ) [  ;check v(S)/|S| against current maximum
      show "If it is the maximun:"
      show "The maximum so far is"
      show ?
      show "with"
      show ((sum_prices) / (length ?))
      set maximum-value precision ((sum_prices) / (length ?)) 2
      set maximum-col ? ] ] 

    set sum_prices 0
    report list maximum-col maximum-value
end

to broadcast [ pair ]
  ask services [ 
    set W lput (item 1 pair) W
    set S lput (item 0 pair) S ]
end

to-report find-index-of-max [ lst ]
  let index 0
  let t 0
  
  set t -1000
  
  foreach lst [
    if( ? > t ) [
      set t ?
      set index (position ? lst) ] ]
  
  report index
end
@#$#@#$#@
GRAPHICS-WINDOW
188
10
935
463
30
17
12.082
1
10
1
1
1
0
0
0
1
-30
30
-17
17
0
0
1
ticks
30.0

SLIDER
12
10
184
43
num-FCs
num-FCs
1
20
10
1
1
NIL
HORIZONTAL

SLIDER
12
76
184
109
num-services
num-services
1
1000
510
1
1
NIL
HORIZONTAL

SLIDER
12
43
184
76
num-needs
num-needs
1
10
1
1
1
NIL
HORIZONTAL

BUTTON
11
116
75
149
Setup
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

OUTPUT
941
10
1251
466
12

MONITOR
79
115
183
160
Num Clusters
count clusters
17
1
11

BUTTON
13
231
184
264
Combine Clusters
compute-coalitions
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
13
276
184
309
Solution bundles
find-co
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
16
186
185
219
Combine Lower Clusters
combine-lower-clusters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
Title: Services v0.10  
Author: Ivan S. Razo-Zapata  
Description:

Solves a 

## HOW IT WORKS

## A.

## HOW TO USE IT

## B.

## THINGS TO NOTICE

## C.


## THINGS TO TRY

## D.


## EXTENDING THE MODEL

## E.


## CREDITS AND REFERENCES

Fundamentals of Multiagent Systems:  
http://jmvidal.cse.sc.edu/library/vidalfmas.pdf

Netlogo Handbook:  
http://ccl.northwestern.edu/netlogo/docs/
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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
