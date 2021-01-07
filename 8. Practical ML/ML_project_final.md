Reading data
------------

    library(tidyverse)

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

    library(data.table)

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

    library(caret)

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

    library(recipes)

    ## 
    ## Attaching package: 'recipes'

    ## The following object is masked from 'package:stringr':
    ## 
    ##     fixed

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

    training <- fread("pml-training.csv")
    testing <- fread("pml-testing.csv")
    #head(training)
    #head(testing)

    dim(training)

    ## [1] 19622   160

    dim(testing)

    ## [1]  20 160

Cleaning Data
-------------

### Clean the Near Zero Variance Variables.

    NZV <- nearZeroVar(training, saveMetrics = TRUE)
    head(NZV, 20)

    ##                      freqRatio percentUnique zeroVar   nzv
    ## V1                    1.000000  100.00000000   FALSE FALSE
    ## user_name             1.100679    0.03057792   FALSE FALSE
    ## raw_timestamp_part_1  1.000000    4.26562022   FALSE FALSE
    ## raw_timestamp_part_2  1.000000   85.53154622   FALSE FALSE
    ## cvtd_timestamp        1.000668    0.10192641   FALSE FALSE
    ## new_window           47.330049    0.01019264   FALSE  TRUE
    ## num_window            1.000000    4.37264295   FALSE FALSE
    ## roll_belt             1.101904    6.77810621   FALSE FALSE
    ## pitch_belt            1.036082    9.37722964   FALSE FALSE
    ## yaw_belt              1.058480    9.97349913   FALSE FALSE
    ## total_accel_belt      1.063160    0.14779329   FALSE FALSE
    ## kurtosis_roll_belt    2.000000    2.01304658   FALSE FALSE
    ## kurtosis_picth_belt   1.333333    1.60534094   FALSE FALSE
    ## kurtosis_yaw_belt     0.000000    0.00000000    TRUE  TRUE
    ## skewness_roll_belt    2.000000    2.00285394   FALSE FALSE
    ## skewness_roll_belt.1  1.333333    1.71236367   FALSE FALSE
    ## skewness_yaw_belt     0.000000    0.00000000    TRUE  TRUE
    ## max_roll_belt         1.000000    0.99378249   FALSE FALSE
    ## max_picth_belt        1.538462    0.11211905   FALSE FALSE
    ## max_yaw_belt          1.034483    0.33635715   FALSE FALSE

    column_near_zero_var <-nearZeroVar(training)
    column_near_zero_var

    ##  [1]   6  14  17  26  51  52  53  54  55  56  57  58  59  75  78  79  81  82  89
    ## [20]  92 101 127 130 131 134 137 139 142 143 144 145 146 147 148 149 150

    length(column_near_zero_var)

    ## [1] 36

    class(column_near_zero_var)

    ## [1] "integer"

    training <- training[,-..column_near_zero_var]
    testing <- testing[,-..column_near_zero_var]

    dim(training)

    ## [1] 19622   124

    dim(testing)

    ## [1]  20 124

### Removing some columns of the dataset that do not contribute much to the accelerometer measurements.

    head(training)

    ##    V1 user_name raw_timestamp_part_1 raw_timestamp_part_2   cvtd_timestamp
    ## 1:  1  carlitos           1323084231               788290 05/12/2011 11:23
    ## 2:  2  carlitos           1323084231               808298 05/12/2011 11:23
    ## 3:  3  carlitos           1323084231               820366 05/12/2011 11:23
    ## 4:  4  carlitos           1323084232               120339 05/12/2011 11:23
    ## 5:  5  carlitos           1323084232               196328 05/12/2011 11:23
    ## 6:  6  carlitos           1323084232               304277 05/12/2011 11:23
    ##    num_window roll_belt pitch_belt yaw_belt total_accel_belt kurtosis_roll_belt
    ## 1:         11      1.41       8.07    -94.4                3                 NA
    ## 2:         11      1.41       8.07    -94.4                3                 NA
    ## 3:         11      1.42       8.07    -94.4                3                 NA
    ## 4:         12      1.48       8.05    -94.4                3                 NA
    ## 5:         12      1.48       8.07    -94.4                3                 NA
    ## 6:         12      1.45       8.06    -94.4                3                 NA
    ##    kurtosis_picth_belt skewness_roll_belt skewness_roll_belt.1 max_roll_belt
    ## 1:                  NA                 NA                   NA            NA
    ## 2:                  NA                 NA                   NA            NA
    ## 3:                  NA                 NA                   NA            NA
    ## 4:                  NA                 NA                   NA            NA
    ## 5:                  NA                 NA                   NA            NA
    ## 6:                  NA                 NA                   NA            NA
    ##    max_picth_belt max_yaw_belt min_roll_belt min_pitch_belt min_yaw_belt
    ## 1:             NA           NA            NA             NA           NA
    ## 2:             NA           NA            NA             NA           NA
    ## 3:             NA           NA            NA             NA           NA
    ## 4:             NA           NA            NA             NA           NA
    ## 5:             NA           NA            NA             NA           NA
    ## 6:             NA           NA            NA             NA           NA
    ##    amplitude_roll_belt amplitude_pitch_belt var_total_accel_belt avg_roll_belt
    ## 1:                  NA                   NA                   NA            NA
    ## 2:                  NA                   NA                   NA            NA
    ## 3:                  NA                   NA                   NA            NA
    ## 4:                  NA                   NA                   NA            NA
    ## 5:                  NA                   NA                   NA            NA
    ## 6:                  NA                   NA                   NA            NA
    ##    stddev_roll_belt var_roll_belt avg_pitch_belt stddev_pitch_belt
    ## 1:               NA            NA             NA                NA
    ## 2:               NA            NA             NA                NA
    ## 3:               NA            NA             NA                NA
    ## 4:               NA            NA             NA                NA
    ## 5:               NA            NA             NA                NA
    ## 6:               NA            NA             NA                NA
    ##    var_pitch_belt avg_yaw_belt stddev_yaw_belt var_yaw_belt gyros_belt_x
    ## 1:             NA           NA              NA           NA         0.00
    ## 2:             NA           NA              NA           NA         0.02
    ## 3:             NA           NA              NA           NA         0.00
    ## 4:             NA           NA              NA           NA         0.02
    ## 5:             NA           NA              NA           NA         0.02
    ## 6:             NA           NA              NA           NA         0.02
    ##    gyros_belt_y gyros_belt_z accel_belt_x accel_belt_y accel_belt_z
    ## 1:         0.00        -0.02          -21            4           22
    ## 2:         0.00        -0.02          -22            4           22
    ## 3:         0.00        -0.02          -20            5           23
    ## 4:         0.00        -0.03          -22            3           21
    ## 5:         0.02        -0.02          -21            2           24
    ## 6:         0.00        -0.02          -21            4           21
    ##    magnet_belt_x magnet_belt_y magnet_belt_z roll_arm pitch_arm yaw_arm
    ## 1:            -3           599          -313     -128      22.5    -161
    ## 2:            -7           608          -311     -128      22.5    -161
    ## 3:            -2           600          -305     -128      22.5    -161
    ## 4:            -6           604          -310     -128      22.1    -161
    ## 5:            -6           600          -302     -128      22.1    -161
    ## 6:             0           603          -312     -128      22.0    -161
    ##    total_accel_arm var_accel_arm gyros_arm_x gyros_arm_y gyros_arm_z
    ## 1:              34            NA        0.00        0.00       -0.02
    ## 2:              34            NA        0.02       -0.02       -0.02
    ## 3:              34            NA        0.02       -0.02       -0.02
    ## 4:              34            NA        0.02       -0.03        0.02
    ## 5:              34            NA        0.00       -0.03        0.00
    ## 6:              34            NA        0.02       -0.03        0.00
    ##    accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z
    ## 1:        -288         109        -123         -368          337          516
    ## 2:        -290         110        -125         -369          337          513
    ## 3:        -289         110        -126         -368          344          513
    ## 4:        -289         111        -123         -372          344          512
    ## 5:        -289         111        -123         -374          337          506
    ## 6:        -289         111        -122         -369          342          513
    ##    kurtosis_roll_arm kurtosis_picth_arm kurtosis_yaw_arm skewness_roll_arm
    ## 1:                NA                 NA               NA                NA
    ## 2:                NA                 NA               NA                NA
    ## 3:                NA                 NA               NA                NA
    ## 4:                NA                 NA               NA                NA
    ## 5:                NA                 NA               NA                NA
    ## 6:                NA                 NA               NA                NA
    ##    skewness_pitch_arm skewness_yaw_arm max_picth_arm max_yaw_arm min_yaw_arm
    ## 1:                 NA               NA            NA          NA          NA
    ## 2:                 NA               NA            NA          NA          NA
    ## 3:                 NA               NA            NA          NA          NA
    ## 4:                 NA               NA            NA          NA          NA
    ## 5:                 NA               NA            NA          NA          NA
    ## 6:                 NA               NA            NA          NA          NA
    ##    amplitude_yaw_arm roll_dumbbell pitch_dumbbell yaw_dumbbell
    ## 1:                NA      13.05217      -70.49400    -84.87394
    ## 2:                NA      13.13074      -70.63751    -84.71065
    ## 3:                NA      12.85075      -70.27812    -85.14078
    ## 4:                NA      13.43120      -70.39379    -84.87363
    ## 5:                NA      13.37872      -70.42856    -84.85306
    ## 6:                NA      13.38246      -70.81759    -84.46500
    ##    kurtosis_roll_dumbbell kurtosis_picth_dumbbell skewness_roll_dumbbell
    ## 1:                     NA                      NA                     NA
    ## 2:                     NA                      NA                     NA
    ## 3:                     NA                      NA                     NA
    ## 4:                     NA                      NA                     NA
    ## 5:                     NA                      NA                     NA
    ## 6:                     NA                      NA                     NA
    ##    skewness_pitch_dumbbell max_roll_dumbbell max_picth_dumbbell
    ## 1:                      NA                NA                 NA
    ## 2:                      NA                NA                 NA
    ## 3:                      NA                NA                 NA
    ## 4:                      NA                NA                 NA
    ## 5:                      NA                NA                 NA
    ## 6:                      NA                NA                 NA
    ##    max_yaw_dumbbell min_roll_dumbbell min_pitch_dumbbell min_yaw_dumbbell
    ## 1:               NA                NA                 NA               NA
    ## 2:               NA                NA                 NA               NA
    ## 3:               NA                NA                 NA               NA
    ## 4:               NA                NA                 NA               NA
    ## 5:               NA                NA                 NA               NA
    ## 6:               NA                NA                 NA               NA
    ##    amplitude_roll_dumbbell amplitude_pitch_dumbbell total_accel_dumbbell
    ## 1:                      NA                       NA                   37
    ## 2:                      NA                       NA                   37
    ## 3:                      NA                       NA                   37
    ## 4:                      NA                       NA                   37
    ## 5:                      NA                       NA                   37
    ## 6:                      NA                       NA                   37
    ##    var_accel_dumbbell avg_roll_dumbbell stddev_roll_dumbbell var_roll_dumbbell
    ## 1:                 NA                NA                   NA                NA
    ## 2:                 NA                NA                   NA                NA
    ## 3:                 NA                NA                   NA                NA
    ## 4:                 NA                NA                   NA                NA
    ## 5:                 NA                NA                   NA                NA
    ## 6:                 NA                NA                   NA                NA
    ##    avg_pitch_dumbbell stddev_pitch_dumbbell var_pitch_dumbbell avg_yaw_dumbbell
    ## 1:                 NA                    NA                 NA               NA
    ## 2:                 NA                    NA                 NA               NA
    ## 3:                 NA                    NA                 NA               NA
    ## 4:                 NA                    NA                 NA               NA
    ## 5:                 NA                    NA                 NA               NA
    ## 6:                 NA                    NA                 NA               NA
    ##    stddev_yaw_dumbbell var_yaw_dumbbell gyros_dumbbell_x gyros_dumbbell_y
    ## 1:                  NA               NA                0            -0.02
    ## 2:                  NA               NA                0            -0.02
    ## 3:                  NA               NA                0            -0.02
    ## 4:                  NA               NA                0            -0.02
    ## 5:                  NA               NA                0            -0.02
    ## 6:                  NA               NA                0            -0.02
    ##    gyros_dumbbell_z accel_dumbbell_x accel_dumbbell_y accel_dumbbell_z
    ## 1:             0.00             -234               47             -271
    ## 2:             0.00             -233               47             -269
    ## 3:             0.00             -232               46             -270
    ## 4:            -0.02             -232               48             -269
    ## 5:             0.00             -233               48             -270
    ## 6:             0.00             -234               48             -269
    ##    magnet_dumbbell_x magnet_dumbbell_y magnet_dumbbell_z roll_forearm
    ## 1:              -559               293               -65         28.4
    ## 2:              -555               296               -64         28.3
    ## 3:              -561               298               -63         28.3
    ## 4:              -552               303               -60         28.1
    ## 5:              -554               292               -68         28.0
    ## 6:              -558               294               -66         27.9
    ##    pitch_forearm yaw_forearm kurtosis_roll_forearm kurtosis_picth_forearm
    ## 1:         -63.9        -153                    NA                     NA
    ## 2:         -63.9        -153                    NA                     NA
    ## 3:         -63.9        -152                    NA                     NA
    ## 4:         -63.9        -152                    NA                     NA
    ## 5:         -63.9        -152                    NA                     NA
    ## 6:         -63.9        -152                    NA                     NA
    ##    skewness_roll_forearm skewness_pitch_forearm max_picth_forearm
    ## 1:                    NA                     NA                NA
    ## 2:                    NA                     NA                NA
    ## 3:                    NA                     NA                NA
    ## 4:                    NA                     NA                NA
    ## 5:                    NA                     NA                NA
    ## 6:                    NA                     NA                NA
    ##    max_yaw_forearm min_pitch_forearm min_yaw_forearm amplitude_pitch_forearm
    ## 1:              NA                NA              NA                      NA
    ## 2:              NA                NA              NA                      NA
    ## 3:              NA                NA              NA                      NA
    ## 4:              NA                NA              NA                      NA
    ## 5:              NA                NA              NA                      NA
    ## 6:              NA                NA              NA                      NA
    ##    total_accel_forearm var_accel_forearm gyros_forearm_x gyros_forearm_y
    ## 1:                  36                NA            0.03            0.00
    ## 2:                  36                NA            0.02            0.00
    ## 3:                  36                NA            0.03           -0.02
    ## 4:                  36                NA            0.02           -0.02
    ## 5:                  36                NA            0.02            0.00
    ## 6:                  36                NA            0.02           -0.02
    ##    gyros_forearm_z accel_forearm_x accel_forearm_y accel_forearm_z
    ## 1:           -0.02             192             203            -215
    ## 2:           -0.02             192             203            -216
    ## 3:            0.00             196             204            -213
    ## 4:            0.00             189             206            -214
    ## 5:           -0.02             189             206            -214
    ## 6:           -0.03             193             203            -215
    ##    magnet_forearm_x magnet_forearm_y magnet_forearm_z classe
    ## 1:              -17              654              476      A
    ## 2:              -18              661              473      A
    ## 3:              -18              658              469      A
    ## 4:              -16              658              469      A
    ## 5:              -17              655              473      A
    ## 6:               -9              660              478      A

    regex <- grepl("^X|timestamp|user_name", names(training))
    #regex

    # training[,.SD,.SDcols=-c(2,3,4,5)]
    training <- training[,.SD,.SDcols=-regex]
    testing <- testing[,.SD,.SDcols=-regex]

    dim(training)

    ## [1] 19622   120

    dim(testing)

    ## [1]  20 120

### Removing columns that contain NA’s.

    # Check which column have na value > 50 %
    col_high_na_value<-training[, which(colMeans(is.na(training)) > 0.5)]
    col_high_na_value

    ##       kurtosis_roll_belt      kurtosis_picth_belt       skewness_roll_belt 
    ##                        7                        8                        9 
    ##     skewness_roll_belt.1            max_roll_belt           max_picth_belt 
    ##                       10                       11                       12 
    ##             max_yaw_belt            min_roll_belt           min_pitch_belt 
    ##                       13                       14                       15 
    ##             min_yaw_belt      amplitude_roll_belt     amplitude_pitch_belt 
    ##                       16                       17                       18 
    ##     var_total_accel_belt            avg_roll_belt         stddev_roll_belt 
    ##                       19                       20                       21 
    ##            var_roll_belt           avg_pitch_belt        stddev_pitch_belt 
    ##                       22                       23                       24 
    ##           var_pitch_belt             avg_yaw_belt          stddev_yaw_belt 
    ##                       25                       26                       27 
    ##             var_yaw_belt            var_accel_arm        kurtosis_roll_arm 
    ##                       28                       42                       52 
    ##       kurtosis_picth_arm         kurtosis_yaw_arm        skewness_roll_arm 
    ##                       53                       54                       55 
    ##       skewness_pitch_arm         skewness_yaw_arm            max_picth_arm 
    ##                       56                       57                       58 
    ##              max_yaw_arm              min_yaw_arm        amplitude_yaw_arm 
    ##                       59                       60                       61 
    ##   kurtosis_roll_dumbbell  kurtosis_picth_dumbbell   skewness_roll_dumbbell 
    ##                       65                       66                       67 
    ##  skewness_pitch_dumbbell        max_roll_dumbbell       max_picth_dumbbell 
    ##                       68                       69                       70 
    ##         max_yaw_dumbbell        min_roll_dumbbell       min_pitch_dumbbell 
    ##                       71                       72                       73 
    ##         min_yaw_dumbbell  amplitude_roll_dumbbell amplitude_pitch_dumbbell 
    ##                       74                       75                       76 
    ##       var_accel_dumbbell        avg_roll_dumbbell     stddev_roll_dumbbell 
    ##                       78                       79                       80 
    ##        var_roll_dumbbell       avg_pitch_dumbbell    stddev_pitch_dumbbell 
    ##                       81                       82                       83 
    ##       var_pitch_dumbbell         avg_yaw_dumbbell      stddev_yaw_dumbbell 
    ##                       84                       85                       86 
    ##         var_yaw_dumbbell    kurtosis_roll_forearm   kurtosis_picth_forearm 
    ##                       87                      100                      101 
    ##    skewness_roll_forearm   skewness_pitch_forearm        max_picth_forearm 
    ##                      102                      103                      104 
    ##          max_yaw_forearm        min_pitch_forearm          min_yaw_forearm 
    ##                      105                      106                      107 
    ##  amplitude_pitch_forearm        var_accel_forearm 
    ##                      108                      110

    training <- training[,.SD,.SDcols=-col_high_na_value]
    testing <- testing[,.SD,.SDcols=-col_high_na_value]

    dim(training)

    ## [1] 19622    55

    dim(testing)

    ## [1] 20 55

Partitioning Training Set
-------------------------

    set.seed(430)
    split = createDataPartition(training$classe, p =0.8, list = FALSE)
    train = training[split, ]
    valid = training[-split, ]
    dim(train)

    ## [1] 15699    55

    dim(valid)

    ## [1] 3923   55

Try lda model (faster method)
-----------------------------

    start_time <- lubridate::minute(Sys.time())
    model_lda <- train(classe ~ ., data = train, method = "lda",verbose=FALSE#trControl=cv
                     )

    end_time <- lubridate::minute(Sys.time())
    time <- (end_time - start_time)
    print(time) # < 1 min

    ## [1] 0

    model_lda

    ## Linear Discriminant Analysis 
    ## 
    ## 15699 samples
    ##    54 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 15699, 15699, 15699, 15699, 15699, 15699, ... 
    ## Resampling results:
    ## 
    ##   Accuracy  Kappa    
    ##   0.99991   0.9998862

    predict<- predict(model_lda, valid[,-c("classe")])
    confusionMatrix(factor(valid$classe), predict)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1116    0    0    0    0
    ##          B    0  759    0    0    0
    ##          C    0    0  684    0    0
    ##          D    0    0    0  643    0
    ##          E    0    0    0    0  721
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9991, 1)
    ##     No Information Rate : 0.2845     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##                                      
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

    #predict(model_lda, testing)
