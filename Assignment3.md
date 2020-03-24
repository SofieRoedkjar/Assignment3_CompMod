Assignment 3 - Exploring causal inference issues
------------------------------------------------

In this assignment we explore some issues related to multiple
regressions (regressions with more than one predictor), and inferred
(causal) relations between variables. N.B. the data is simulated (to
make sure I know the actual mechanism generating it), but it’s based on
a real study. So bear with a longish introduction to get into the
details of what we are doing and why it is important.

### Altercentric intrusion in schizophrenia

People with schizophrenia often report altered control and distinction
of self-other representations: intrusive thoughts, hearing of voices,
delusions of mind reading, paranoia, etc (a substantial portion of the
psychotic symptoms experienced in schizophrenia). These have been
variously attributed to hypermentalizing (over attribution of mental
states to others), social impairment (over preoccupation with own
thought processes), hyper socialization (inability to inhibit
information from others), etc.

The current study investigates 1) whether schizophrenia is indeed
related to altered control and distinction of self-other
representations, in particular altercentric intrusions (inability to
inhibit social information), and 2) whether these are related to the
relevant psychotic symptoms. N.B. the actual study also investigates
egocentric intrusion, do check the papers below if interested.

The task is a slightly modified version of this:
<a href="https://www.ncbi.nlm.nih.gov/pubmed/20731512" class="uri">https://www.ncbi.nlm.nih.gov/pubmed/20731512</a>
You look at a picture with some dots visible to you, as well as with a
different person with a different set of dots visible to them. The
number of dots you see and that the other sees can be the same
(congruent condition) or not (incongruent condition). You are tasked to
indicate whether a given number (e.g. 3) matches the number of dots you
see (and the dots visible to the other person are irrelevant to the
task).

The tasks investigates altercentric intrusion: will your reaction time
change according to whether the other person is seeing the same amount
of dots as you, or not? The idea is that if you correctly inhibit social
information, your reaction time should not change, as the information
about the other person is not relevant. On the contrary, if you
nevertheless use task irrelevant social information, you’ll be slower at
indicating whether 3 is the right number of dots when the other person
sees a different amount of dots than you (conflicting information). The
bigger the difference between RTs in the congruent and incongruent
condition the bigger the altercentric intrusion effect.

For each participant you have 6 variables: 1) ID, 2)
AltercentricIntrusion (continuous score), 3) Diagnosis (schizophrenia
vs. control), 4) VoiceHearing (severity of voice hearing symptoms,
continuous score of the severity of the symptom as measured by a
clinician), 5) MindReading (severity of delusions of mind reading,
continuous score of the severity of the symptom as measured by a
clinician); 6) Apathy (severity of lack of motivation in taking care of
oneself, from washing to showing up at work, continuous score of the
severity of the symptom as measured by a clinician).

The research questions you have to answer are the following:

First part
----------

Q1.1) Does schizophrenia involved altercentric intrusion? Define model
and priors. Test the implications of your priors (prior predictive
checks) and if needed adjust them. Run the model. Test the quality of
the fitted model (posterior predictive checks). Assess the evidence in
favor of an increased altercentric intrusion in schizophrenia. Report
the model and the results, including plots.

``` r
pacman::p_load(tidyverse, brms)

# Prepare the data
d <- read_csv("Ass3.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   VoiceHearing = col_double(),
    ##   MindReading = col_double(),
    ##   Apathy = col_double(),
    ##   AltercentricIntrusion = col_double(),
    ##   ID = col_double(),
    ##   Diagnosis = col_double()
    ## )

``` r
summary(d)
```

    ##   VoiceHearing      MindReading          Apathy       
    ##  Min.   :-1.8754   Min.   :-1.4875   Min.   :-1.4747  
    ##  1st Qu.: 0.5295   1st Qu.: 0.4483   1st Qu.: 0.5856  
    ##  Median : 1.1920   Median : 1.1987   Median : 1.1123  
    ##  Mean   : 1.1780   Mean   : 1.1320   Mean   : 1.1911  
    ##  3rd Qu.: 1.8583   3rd Qu.: 1.7438   3rd Qu.: 1.8822  
    ##  Max.   : 3.6905   Max.   : 3.7404   Max.   : 3.5015  
    ##  AltercentricIntrusion       ID           Diagnosis   
    ##  Min.   :1.494         Min.   :  1.00   Min.   :0.00  
    ##  1st Qu.:3.322         1st Qu.: 75.75   1st Qu.:0.00  
    ##  Median :4.046         Median :150.50   Median :0.00  
    ##  Mean   :3.952         Mean   :150.50   Mean   :0.25  
    ##  3rd Qu.:4.611         3rd Qu.:225.25   3rd Qu.:0.25  
    ##  Max.   :6.312         Max.   :300.00   Max.   :1.00

``` r
#make 0 controls and 1 Schizophrenia
d$Diagnosis <- plyr::revalue(as.character(d$Diagnosis), 
                             c("0"="Controls", "1"="Schizophrenia"))

d <- d %>%
  mutate(
    ID = as.factor(ID),
    Diagnosis = as.factor(Diagnosis)
  )

# Define the formula (bf = bayesian formula)
AltercentricDiagnosis_f0 <- bf(
  AltercentricIntrusion ~ 1 + Diagnosis
)

AltercentricDiagnosis_f <- bf(
  AltercentricIntrusion ~ 0 + Diagnosis
)

# Design the priors
get_prior(AltercentricDiagnosis_f0, family = gaussian, d)
```

    ##                 prior     class                   coef group resp dpar
    ## 1                             b                                       
    ## 2                             b DiagnosisSchizophrenia                
    ## 3 student_t(3, 4, 10) Intercept                                       
    ## 4 student_t(3, 0, 10)     sigma                                       
    ##   nlpar bound
    ## 1            
    ## 2            
    ## 3            
    ## 4

``` r
get_prior(AltercentricDiagnosis_f, family = gaussian, d) #output tells us that we can have 3 betas and a sigma
```

    ##                 prior class                   coef group resp dpar nlpar
    ## 1                         b                                             
    ## 2                         b      DiagnosisControls                      
    ## 3                         b DiagnosisSchizophrenia                      
    ## 4 student_t(3, 0, 10) sigma                                             
    ##   bound
    ## 1      
    ## 2      
    ## 3      
    ## 4

``` r
  #one combined beta, or one for each of them

#define priors (beta and sigma)
priorDiagnosis <- c(
  prior(normal(4, 1), class = b),
  prior(normal(1, 2), class = sigma)
) 

# Test the priors
AltercentricDiagnosis_PriorCheck_m <- brm(
  formula = AltercentricDiagnosis_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only" #you dont want to run the model, only test the priors
)
```

    ## Compiling the C++ model

    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.037 seconds (Warm-up)
    ## Chain 1:                0.032 seconds (Sampling)
    ## Chain 1:                0.069 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.055 seconds (Warm-up)
    ## Chain 2:                0.025 seconds (Sampling)
    ## Chain 2:                0.08 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.054 seconds (Warm-up)
    ## Chain 3:                0.053 seconds (Sampling)
    ## Chain 3:                0.107 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '58ee1ee27bbbc25da51be768bc98b3fb' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.025 seconds (Warm-up)
    ## Chain 4:                0.047 seconds (Sampling)
    ## Chain 4:                0.072 seconds (Total)
    ## Chain 4:

``` r
pp_check(AltercentricDiagnosis_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
## Fitting the model
AltercentricDiagnosis_m <- brm(
  formula = AltercentricDiagnosis_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T #actually fitting the model (sample_prior = TRUE)
)
```

    ## Compiling the C++ model
    ## Start sampling

    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.068 seconds (Warm-up)
    ## Chain 1:                0.054 seconds (Sampling)
    ## Chain 1:                0.122 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.078 seconds (Warm-up)
    ## Chain 2:                0.124 seconds (Sampling)
    ## Chain 2:                0.202 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.07 seconds (Warm-up)
    ## Chain 3:                0.119 seconds (Sampling)
    ## Chain 3:                0.189 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL '250368a6e8ef8b9b74282e648bfa3499' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.084 seconds (Warm-up)
    ## Chain 4:                0.215 seconds (Sampling)
    ## Chain 4:                0.299 seconds (Total)
    ## Chain 4:

``` r
# Posterior predictive check
pp_check(AltercentricDiagnosis_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
#the dark blue line is the acutal data
#the light blue lines are simulated data using the model

## Check the model for warnings and get model output
AltercentricDiagnosis_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 0 + Diagnosis 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## DiagnosisControls          3.86      0.06     3.75     3.98 1.00     3643
    ## DiagnosisSchizophrenia     4.22      0.11     4.02     4.43 1.00     3457
    ##                        Tail_ESS
    ## DiagnosisControls          2679
    ## DiagnosisSchizophrenia     2988
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.91      0.04     0.84     0.99 1.00     3733     2949
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Hypothesis testing + updating check
plot(hypothesis(AltercentricDiagnosis_m,
           "DiagnosisSchizophrenia > DiagnosisControls"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
hypothesis(AltercentricDiagnosis_m,
           "DiagnosisSchizophrenia > DiagnosisControls")
```

    ## Hypothesis Tests for class b:
    ##                 Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (DiagnosisSchizop... > 0     0.36      0.12     0.16     0.56        999
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#the estimate is the difference between the two groups

conditional_effects(AltercentricDiagnosis_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
plot(conditional_effects(AltercentricDiagnosis_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-1-5.png)

``` r
#The model indicates a credible difference in altercentric intrusion in the two groups supporting our hypothesis #(b = 0.36, CIs = 0.16, 0.57, ER = 1332). Controls showed on average an altercentric intrusion effect of 3.86 #(CIs 3.74, 3.98), and schizophrenia of 4.22 (CIs = 4.01, 4.43).
#[Add plot of the effects]

#SI
#The model had no divergences, a Rhat of 1, and Effective Sample Sizes above 2000 for both Bulk and Tail.
#[Add prior and posterior checks plots; add updating check plot]
```

Q1.2) Is altercentric intrusion related to specific symptoms *in the
patients*? Identify which of the symptoms could be relevant. Should you
include more than one symptom? Build models, priors, predictive checks.
Assess the evidence and report models and results, including plots.
Discuss whether the results make sense.

``` r
#Scale variables
d <- d %>% 
  mutate(
    AltercentricIntrusion = scale(AltercentricIntrusion),
    VoiceHearing = scale(VoiceHearing),
    MindReading = scale(MindReading),
    Apathy = scale(Apathy)
  )
  

##### Define formulas ######
#subset with only SCZ patients
SCZ <- subset(d, Diagnosis == "Schizophrenia")
summary(SCZ)
```

    ##    VoiceHearing.V1       MindReading.V1         Apathy.V1      
    ##  Min.   :-0.4631064   Min.   :-1.5752946   Min.   :-0.6603158  
    ##  1st Qu.: 0.5537819   1st Qu.: 0.0749862   1st Qu.: 0.4945482  
    ##  Median : 0.9644676   Median : 0.6298221   Median : 0.9923623  
    ##  Mean   : 0.9876248   Mean   : 0.6923401   Mean   : 0.9391452  
    ##  3rd Qu.: 1.5582076   3rd Qu.: 1.3848345   3rd Qu.: 1.4869851  
    ##  Max.   : 2.4340907   Max.   : 2.6852970   Max.   : 2.3962430  
    ##                                                                
    ##  AltercentricIntrusion.V1       ID             Diagnosis 
    ##  Min.   :-1.6915010       3      : 1   Controls     : 0  
    ##  1st Qu.:-0.2643837       4      : 1   Schizophrenia:75  
    ##  Median : 0.2084990       5      : 1                     
    ##  Mean   : 0.2973705       16     : 1                     
    ##  3rd Qu.: 0.8249317       18     : 1                     
    ##  Max.   : 2.5559055       23     : 1                     
    ##                           (Other):69

``` r
# Define the formula (bf = bayesian formula)
VoiceHearing_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing
)

MindReading_f <- bf(
  AltercentricIntrusion ~ 1 + MindReading
)

Apathy_f <- bf(
  AltercentricIntrusion ~ 1 + Apathy
)

get_prior(VoiceHearing_f, family = gaussian, d)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b VoiceHearing                            
    ## 3 student_t(3, 0, 10) Intercept                                         
    ## 4 student_t(3, 0, 10)     sigma

``` r
#define priors (beta and sigma) - it is based on the outcome-variable and therfore stays the same
priorDiagnosis <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 3), class = b),
  prior(normal(1, 2), class = sigma)
)

# Test the priors for VoiceHearing_m
VoiceHearing_PriorCheck_m <- brm(
  formula = VoiceHearing_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "Altercentric_Voice_p"
)

pp_check(VoiceHearing_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
## Fitting the model VoiceHearing_m
VoiceHearing_m <- brm(
  formula = VoiceHearing_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "Altercentric_Voice_m"
)

# Posterior predictive check
pp_check(VoiceHearing_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
#the dark blue line is the acutal data
#the light blue lines are simulated data using the model

## Check the model for warnings and get model output
VoiceHearing_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing 
    ##    Data: SCZ (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.21      0.18    -0.15     0.57 1.00     3717     3138
    ## VoiceHearing     0.08      0.15    -0.21     0.37 1.00     3624     2765
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.91      0.08     0.77     1.07 1.00     3027     2585
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Test the priors for MindReading
MindReading_PriorCheck_m <- brm(
  formula = MindReading_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "Altercentric_Mind_p"
)

pp_check(MindReading_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
## Fitting the model VoiceHearing_m
MindReading_m <- brm(
  formula = MindReading_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "Altercentric_Mind_m"
)

# Posterior predictive check
pp_check(MindReading_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-4.png)

``` r
## Check the model for warnings and get model output
MindReading_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + MindReading 
    ##    Data: SCZ (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       0.24      0.13    -0.01     0.49 1.00     3752     2833
    ## MindReading     0.08      0.11    -0.14     0.31 1.00     3953     2784
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.07     0.77     1.06 1.00     3186     2904
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Test the priors for Apathy
Apathy_PriorCheck_m <- brm(
  formula = Apathy_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "Altercentric_Apathy_p"
)

pp_check(Apathy_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-5.png)

``` r
## Fitting the model VoiceHearing_m
Apathy_m <- brm(
  formula = Apathy_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "Altercentric_Apathy_m"
)

# Posterior predictive check
pp_check(Apathy_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-6.png)

``` r
## Check the model for warnings and get model output
Apathy_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + Apathy 
    ##    Data: SCZ (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.52      0.17     0.21     0.86 1.00     3998     3289
    ## Apathy       -0.24      0.14    -0.53     0.03 1.00     3774     2658
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.89      0.08     0.76     1.05 1.00     3812     2612
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
########## HYPOTHESIS TESTING #############
# Hypothesis testing + updating check
plot(hypothesis(VoiceHearing_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-7.png)

``` r
hypothesis(VoiceHearing_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (VoiceHearing) > 0     0.08      0.15    -0.16     0.33       2.41
    ##   Post.Prob Star
    ## 1      0.71     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#the estimate is the difference between the two groups
conditional_effects(VoiceHearing_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-8.png)

``` r
plot(conditional_effects(VoiceHearing_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-9.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(MindReading_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-10.png)

``` r
hypothesis(MindReading_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (MindReading) > 0     0.08      0.11    -0.11     0.27       3.22
    ##   Post.Prob Star
    ## 1      0.76     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#the estimate is the difference between the two groups
conditional_effects(MindReading_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-11.png)

``` r
plot(conditional_effects(MindReading_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-12.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(Apathy_m,
           "Apathy > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-13.png)

``` r
hypothesis(Apathy_m,
           "Apathy > 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (Apathy) > 0    -0.24      0.14    -0.48    -0.01       0.05      0.04
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#the estimate is the difference between the two groups
conditional_effects(Apathy_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-14.png)

``` r
plot(conditional_effects(Apathy_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-15.png)

``` r
######## TWO PREDICTORS ###############

# Define the formula (bf = bayesian formula)
Voice_Mind_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing + MindReading
)

Voice_Mind_Apathy_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing + MindReading + Apathy
)

# Test the priors for VoiceHearing_m
Voice_Mind_PriorCheck_m <- brm(
  formula = Voice_Mind_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "Voice_Mind_p"
)

pp_check(Voice_Mind_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-16.png)

``` r
## Fitting the model VoiceHearing_m
Voice_Mind_m <- brm(
  formula = Voice_Mind_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "Voice_Mind_m"
)

# Posterior predictive check
pp_check(Voice_Mind_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-17.png)

``` r
#the dark blue line is the acutal data
#the light blue lines are simulated data using the model



# Test the priors for VoiceHearing_m
Voice_Mind_Apathy_PriorCheck_m <- brm(
  formula = Voice_Mind_Apathy_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "Voice_Mind_Apathy_p1"
)

pp_check(Voice_Mind_Apathy_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-18.png)

``` r
## Fitting the model VoiceHearing_m
Voice_Mind_Apathy_m <- brm(
  formula = Voice_Mind_Apathy_f,
  data = SCZ,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T,
  file = "Voice_Mind_Apathy_m"
)

# Posterior predictive check
pp_check(Voice_Mind_Apathy_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-19.png)

``` r
## Check the model for warnings and get model output
Voice_Mind_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading 
    ##    Data: SCZ (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.07      0.23    -0.40     0.53 1.00     3677     2838
    ## VoiceHearing     0.14      0.16    -0.17     0.47 1.00     3989     3069
    ## MindReading      0.13      0.13    -0.12     0.36 1.00     3812     3059
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.08     0.77     1.07 1.00     3403     2803
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
Voice_Mind_Apathy_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading + Apathy 
    ##    Data: SCZ (Number of observations: 75) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.41      0.38    -0.33     1.15 1.00     2840     2774
    ## VoiceHearing     0.06      0.18    -0.30     0.41 1.00     3394     2907
    ## MindReading      0.03      0.15    -0.25     0.33 1.00     3054     2674
    ## Apathy          -0.21      0.17    -0.54     0.13 1.00     3184     3266
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.90      0.08     0.76     1.08 1.00     3893     2849
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Hypothesis testing: If we already know MindReading, does VoiceHearing then add anything
plot(hypothesis(Voice_Mind_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-20.png)

``` r
hypothesis(Voice_Mind_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (VoiceHearing) > 0     0.14      0.16    -0.12     0.42       4.46
    ##   Post.Prob Star
    ## 1      0.82     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#hypothesis testing: If we already know VoiceHearing, does MindReading add anything
plot(hypothesis(Voice_Mind_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-21.png)

``` r
hypothesis(Voice_Mind_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (MindReading) > 0     0.13      0.13    -0.08     0.32       5.23
    ##   Post.Prob Star
    ## 1      0.84     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# Hypothesis testing: If we already know Apathy and MindReading, does VoiceHearing then add anything
plot(hypothesis(Voice_Mind_Apathy_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-22.png)

``` r
hypothesis(Voice_Mind_Apathy_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (VoiceHearing) > 0     0.06      0.18    -0.24     0.35       1.71
    ##   Post.Prob Star
    ## 1      0.63     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#hypothesis testing: If we already know Apathy and VoiceHearing, does MindReading add anything
plot(hypothesis(Voice_Mind_Apathy_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-23.png)

``` r
hypothesis(Voice_Mind_Apathy_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (MindReading) > 0     0.03      0.15    -0.21     0.28       1.43
    ##   Post.Prob Star
    ## 1      0.59     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#hypothesis testing: If we already know MindReading and VoiceHearing, does Apathy add anything
plot(hypothesis(Voice_Mind_Apathy_m,
           "Apathy > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-24.png)

``` r
hypothesis(Voice_Mind_Apathy_m,
           "Apathy > 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (Apathy) > 0    -0.21      0.17    -0.49     0.08       0.13      0.11
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
conditional_effects(Voice_Mind_Apathy_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-25.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-26.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-27.png)

``` r
plot(conditional_effects(Voice_Mind_Apathy_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-28.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-29.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-2-30.png)

``` r
######## CHECKING FOR OVERFITTING ###########
VoiceHearing_m <- add_criterion(VoiceHearing_m, criterion = "loo")
```

    ## Automatically saving the model object in 'Altercentric_Voice_m.rds'

``` r
MindReading_m <- add_criterion(MindReading_m, criterion = "loo")
```

    ## Automatically saving the model object in 'Altercentric_Mind_m.rds'

``` r
Apathy_m <- add_criterion(Apathy_m, criterion = "loo")
```

    ## Automatically saving the model object in 'Altercentric_Apathy_m.rds'

``` r
Voice_Mind_m <- add_criterion(Voice_Mind_m, criterion = "loo")
```

    ## Automatically saving the model object in 'Voice_Mind_m.rds'

``` r
Voice_Mind_Apathy_m <- add_criterion(Voice_Mind_Apathy_m, criterion = "loo")
```

    ## Automatically saving the model object in 'Voice_Mind_Apathy_m.rds'

``` r
loo_compare(VoiceHearing_m, MindReading_m, Apathy_m, Voice_Mind_m, Voice_Mind_Apathy_m)
```

    ##                     elpd_diff se_diff
    ## Apathy_m             0.0       0.0   
    ## MindReading_m       -1.0       1.6   
    ## VoiceHearing_m      -1.5       1.7   
    ## Voice_Mind_m        -1.7       1.5   
    ## Voice_Mind_Apathy_m -2.1       0.4

``` r
loo_model_weights(VoiceHearing_m, MindReading_m, Apathy_m, Voice_Mind_m, Voice_Mind_Apathy_m)
```

    ## Method: stacking
    ## ------
    ##                     weight
    ## VoiceHearing_m      0.000 
    ## MindReading_m       0.140 
    ## Apathy_m            0.860 
    ## Voice_Mind_m        0.000 
    ## Voice_Mind_Apathy_m 0.000

Second part
-----------

Q2.1) However, we know that the diagnosis is based on symptom
assessment: if the overall sum of symptoms is severe enough, the
participant gets a diagnosis. In other words, by selecting the patients,
and including the symptoms in the model we might have inadvertently
introduced an issue in our inference. Do try to draw a causal graph
(Directed Acyclical Graph) of the variables and compare it with the
types of causal graphs presented in the slides. Discuss which biases you
might have introduced.

Q2.2.) Redesign your analysis following the graph and report how the
results change

``` r
########## NOW WITH ALL PARTICIPANTS #############

# Define the formula (bf = bayesian formula)
VoiceHearing_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing
)

MindReading_f <- bf(
  AltercentricIntrusion ~ 1 + MindReading
)

Apathy_f <- bf(
  AltercentricIntrusion ~ 1 + Apathy
)

get_prior(VoiceHearing_f, family = gaussian, d)
```

    ##                 prior     class         coef group resp dpar nlpar bound
    ## 1                             b                                         
    ## 2                             b VoiceHearing                            
    ## 3 student_t(3, 0, 10) Intercept                                         
    ## 4 student_t(3, 0, 10)     sigma

``` r
#define priors (beta and sigma) - it is based on the outcome-variable and therfore stays the same
priorDiagnosis <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 3), class = b),
  prior(normal(1, 2), class = sigma)
)

# Test the priors for VoiceHearing_m
all_VoiceHearing_PriorCheck_m <- brm(
  formula = VoiceHearing_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "all_Altercentric_Voice_p"
)

pp_check(all_VoiceHearing_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
## Fitting the model VoiceHearing_m
all_VoiceHearing_m <- brm(
  formula = VoiceHearing_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "all_Altercentric_Voice_m"
)

# Posterior predictive check
pp_check(all_VoiceHearing_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
#the dark blue line is the acutal data
#the light blue lines are simulated data using the model

## Check the model for warnings and get model output
all_VoiceHearing_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       -0.00      0.06    -0.11     0.11 1.00     4354     2994
    ## VoiceHearing     0.20      0.06     0.08     0.31 1.00     4604     3131
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.99      0.04     0.91     1.07 1.00     4397     2882
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Test the priors for MindReading
all_MindReading_PriorCheck_m <- brm(
  formula = MindReading_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "all_Altercentric_Mind_p"
)

pp_check(all_MindReading_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-3.png)

``` r
## Fitting the model VoiceHearing_m
all_MindReading_m <- brm(
  formula = MindReading_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "all_Altercentric_Mind_m"
)

# Posterior predictive check
pp_check(all_MindReading_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-4.png)

``` r
## Check the model for warnings and get model output
all_MindReading_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + MindReading 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       0.00      0.06    -0.11     0.12 1.00     3519     2772
    ## MindReading     0.19      0.06     0.08     0.30 1.00     3878     2906
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.99      0.04     0.91     1.07 1.00     3281     2950
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Test the priors for Apathy
all_Apathy_PriorCheck_m <- brm(
  formula = Apathy_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "all_Altercentric_Apathy_p"
)

pp_check(all_Apathy_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-5.png)

``` r
## Fitting the model VoiceHearing_m
all_Apathy_m <- brm(
  formula = Apathy_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "all_Altercentric_Apathy_m"
)

# Posterior predictive check
pp_check(all_Apathy_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-6.png)

``` r
## Check the model for warnings and get model output
all_Apathy_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + Apathy 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept     0.00      0.06    -0.12     0.12 1.00     3880     2459
    ## Apathy        0.09      0.06    -0.03     0.20 1.00     3713     3192
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     1.00      0.04     0.93     1.09 1.00     3409     2654
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
########## HYPOTHESIS TESTING #############
# Hypothesis testing + updating check
plot(hypothesis(all_VoiceHearing_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-7.png)

``` r
hypothesis(all_VoiceHearing_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (VoiceHearing) > 0      0.2      0.06      0.1     0.29       3999
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#the estimate is the difference between the two groups
conditional_effects(all_VoiceHearing_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-8.png)

``` r
plot(conditional_effects(all_VoiceHearing_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-9.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(all_MindReading_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-10.png)

``` r
hypothesis(all_MindReading_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (MindReading) > 0     0.19      0.06      0.1     0.29        Inf
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#the estimate is the difference between the two groups
conditional_effects(all_MindReading_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-11.png)

``` r
plot(conditional_effects(all_MindReading_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-12.png)

``` r
# Hypothesis testing + updating check
plot(hypothesis(all_Apathy_m,
           "Apathy > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-13.png)

``` r
hypothesis(all_Apathy_m,
           "Apathy > 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (Apathy) > 0     0.09      0.06    -0.01     0.18      15.19      0.94
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#the estimate is the difference between the two groups
conditional_effects(all_Apathy_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-14.png)

``` r
plot(conditional_effects(all_Apathy_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-15.png)

``` r
######## TWO PREDICTORS ###############

# Define the formula (bf = bayesian formula)
Voice_Mind_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing + MindReading
)

Voice_Mind_Apathy_f <- bf(
  AltercentricIntrusion ~ 1 + VoiceHearing + MindReading + Apathy
)

# Test the priors for VoiceHearing_m
all_Voice_Mind_PriorCheck_m <- brm(
  formula = Voice_Mind_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "all_Voice_Mind_p"
)

pp_check(all_Voice_Mind_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-16.png)

``` r
## Fitting the model VoiceHearing_m
all_Voice_Mind_m <- brm(
  formula = Voice_Mind_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T, #actually fitting the model (sample_prior = TRUE)
  file = "all_Voice_Mind_m"
)

# Posterior predictive check
pp_check(all_Voice_Mind_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-17.png)

``` r
#the dark blue line is the acutal data
#the light blue lines are simulated data using the model



# Test the priors for VoiceHearing_m
all_Voice_Mind_Apathy_PriorCheck_m <- brm(
  formula = Voice_Mind_Apathy_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = "only", #you dont want to run the model, only test the priors
  file = "all_Voice_Mind_Apathy_p1"
)

pp_check(all_Voice_Mind_Apathy_PriorCheck_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-18.png)

``` r
## Fitting the model VoiceHearing_m
all_Voice_Mind_Apathy_m <- brm(
  formula = Voice_Mind_Apathy_f,
  data = d,
  family = gaussian,
  prior = priorDiagnosis,
  sample_prior = T,
  file = "all_Voice_Mind_Apathy_m"
)

# Posterior predictive check
pp_check(all_Voice_Mind_Apathy_m, nsamples = 100)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-19.png)

``` r
## Check the model for warnings and get model output
all_Voice_Mind_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        0.00      0.06    -0.11     0.11 1.00     3750     2623
    ## VoiceHearing     0.17      0.06     0.06     0.28 1.00     4396     3139
    ## MindReading      0.17      0.06     0.06     0.28 1.00     4122     2934
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.97      0.04     0.90     1.06 1.00     4529     2993
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
all_Voice_Mind_Apathy_m
```

    ##  Family: gaussian 
    ##   Links: mu = identity; sigma = identity 
    ## Formula: AltercentricIntrusion ~ 1 + VoiceHearing + MindReading + Apathy 
    ##    Data: d (Number of observations: 300) 
    ## Samples: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 4000
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept       -0.00      0.06    -0.11     0.11 1.00     4544     2941
    ## VoiceHearing     0.17      0.06     0.05     0.28 1.00     4483     3186
    ## MindReading      0.17      0.06     0.06     0.28 1.00     4167     3058
    ## Apathy           0.01      0.06    -0.10     0.13 1.00     4572     2948
    ## 
    ## Family Specific Parameters: 
    ##       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sigma     0.97      0.04     0.90     1.06 1.00     4862     3396
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
# Hypothesis testing: If we already know MindReading, does VoiceHearing then add anything
plot(hypothesis(all_Voice_Mind_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-20.png)

``` r
hypothesis(all_Voice_Mind_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (VoiceHearing) > 0     0.17      0.06     0.08     0.26     570.43
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#hypothesis testing: If we already know VoiceHearing, does MindReading add anything
plot(hypothesis(all_Voice_Mind_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-21.png)

``` r
hypothesis(all_Voice_Mind_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (MindReading) > 0     0.17      0.06     0.08     0.26     570.43
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
# Hypothesis testing: If we already know Apathy and MindReading, does VoiceHearing then add anything
plot(hypothesis(all_Voice_Mind_Apathy_m,
           "VoiceHearing > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-22.png)

``` r
hypothesis(all_Voice_Mind_Apathy_m,
           "VoiceHearing > 0")
```

    ## Hypothesis Tests for class b:
    ##           Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (VoiceHearing) > 0     0.17      0.06     0.07     0.27    1332.33
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#hypothesis testing: If we already know Apathy and VoiceHearing, does MindReading add anything
plot(hypothesis(all_Voice_Mind_Apathy_m,
           "MindReading > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-23.png)

``` r
hypothesis(all_Voice_Mind_Apathy_m,
           "MindReading > 0")
```

    ## Hypothesis Tests for class b:
    ##          Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio
    ## 1 (MindReading) > 0     0.17      0.06     0.07     0.26        499
    ##   Post.Prob Star
    ## 1         1    *
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
#hypothesis testing: If we already know MindReading and VoiceHearing, does Apathy add anything
plot(hypothesis(all_Voice_Mind_Apathy_m,
           "Apathy > 0"))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-24.png)

``` r
hypothesis(all_Voice_Mind_Apathy_m,
           "Apathy > 0")
```

    ## Hypothesis Tests for class b:
    ##     Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob
    ## 1 (Apathy) > 0     0.01      0.06    -0.08     0.11       1.58      0.61
    ##   Star
    ## 1     
    ## ---
    ## 'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
    ## '*': For one-sided hypotheses, the posterior probability exceeds 95%;
    ## for two-sided hypotheses, the value tested against lies outside the 95%-CI.
    ## Posterior probabilities of point hypotheses assume equal prior probabilities.

``` r
conditional_effects(all_Voice_Mind_Apathy_m)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-25.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-26.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-27.png)

``` r
plot(conditional_effects(all_Voice_Mind_Apathy_m), points=T)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-28.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-29.png)![](Assignment3_files/figure-markdown_github/unnamed-chunk-3-30.png)

``` r
######## CHECKING FOR OVERFITTING ###########
all_VoiceHearing_m <- add_criterion(all_VoiceHearing_m, criterion = "loo")
```

    ## Automatically saving the model object in 'all_Altercentric_Voice_m.rds'

``` r
all_MindReading_m <- add_criterion(all_MindReading_m, criterion = "loo")
```

    ## Automatically saving the model object in 'all_Altercentric_Mind_m.rds'

``` r
all_Apathy_m <- add_criterion(all_Apathy_m, criterion = "loo")
```

    ## Automatically saving the model object in 'all_Altercentric_Apathy_m.rds'

``` r
all_Voice_Mind_m <- add_criterion(all_Voice_Mind_m, criterion = "loo")
```

    ## Automatically saving the model object in 'all_Voice_Mind_m.rds'

``` r
all_Voice_Mind_Apathy_m <- add_criterion(all_Voice_Mind_Apathy_m, criterion = "loo")
```

    ## Automatically saving the model object in 'all_Voice_Mind_Apathy_m.rds'

``` r
loo_compare(all_VoiceHearing_m, all_MindReading_m, all_Apathy_m, all_Voice_Mind_m, all_Voice_Mind_Apathy_m)
```

    ##                         elpd_diff se_diff
    ## all_Voice_Mind_m         0.0       0.0   
    ## all_Voice_Mind_Apathy_m -1.0       0.2   
    ## all_VoiceHearing_m      -3.5       2.9   
    ## all_MindReading_m       -3.5       3.1   
    ## all_Apathy_m            -8.1       4.5

``` r
loo_model_weights(all_VoiceHearing_m, all_MindReading_m, all_Apathy_m, all_Voice_Mind_m, all_Voice_Mind_Apathy_m)
```

    ## Method: stacking
    ## ------
    ##                         weight
    ## all_VoiceHearing_m      0.107 
    ## all_MindReading_m       0.150 
    ## all_Apathy_m            0.000 
    ## all_Voice_Mind_m        0.743 
    ## all_Voice_Mind_Apathy_m 0.000

Third part
----------

These issues are very difficult to think through, and not knowing the
causal mechanisms generating the data in advance makes our inferences
even more unreliable. To explore these issues, I recommend using
simulations. In other words, defining a “true” model, generating data
from it and assessing what different analyses would lead you to infer
(and therefore which biases they might introduce). You can find the code
I used to simulate your data below.

Q3.1) Look through the code and identify whether the results you have
match the underlying truth. Discuss what you have learned.

Q3.2) OPTIONAL: is this a general pattern? Try varying the parameters
(e.g. correlation values) and assess whether the new dataset(s) leads to
the same biases in your analysis.

``` r
#Simulating the data
pacman::p_load(MASS, tidyverse, psych)

seed <- 1981 # Defining a seed so the results are always the same
n <- 300 # Defining the amount of participants

SymptomCorr <- .2 # Defining the correlation of symptoms (as they tend to co-occur)
EffectCorrRel <- .2 # Defining the correlation between relevant symptoms and effect (Some symptoms are positively correlated with the effect)
EffectCorrIrrel <- 0 # Defining the correlation between irrelevant symptoms and effect (none)

# Creating the variance-covariance matrix for the variables we want to generate (3 symptoms, 1 effect)
Sigma <- matrix(data=c(1,SymptomCorr,SymptomCorr,EffectCorrRel,
                       SymptomCorr,1,SymptomCorr,EffectCorrRel,
                       SymptomCorr,SymptomCorr,1,EffectCorrIrrel,
                       EffectCorrRel,EffectCorrRel,EffectCorrIrrel,1),
                       nrow=4,ncol=4)

## Generate data from a multivariate (mvr) normal (n) distribution
d <- mvrnorm(n = n, # number of participant
        mu = c(1.2, 1.2, 1.2, 4), # mean of each variable
        Sigma) # variance co-variance matrix

# Giving meaningful names to variables and add ID
d <- data.frame(
  VoiceHearing = d[,1], 
  MindReading =  d[,2],
  Apathy =  d[,3], 
  AltercentricIntrusion = d[,4],
  ID = seq(nrow(d)))

# Assessing whether the participant has schizophrenia (high enough sum of symptoms)
# Here we choose participants scoring above 75% percentile (the most severe ones)
d$Diagnosis <- 0
d$Diagnosis[(d$VoiceHearing + d$MindReading + d$Apathy) > 
              quantile(d$VoiceHearing + d$MindReading + d$Apathy, .75)] <-1

## Plotting the relation between variables in schizophrenia
d1 <- d %>% subset(Diagnosis==1) %>% dplyr::select(-Diagnosis, -ID)
pairs.panels(d1)
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
## Plotting the relation between variables all participants
pairs.panels(dplyr::select(d,-Diagnosis, -ID))
```

![](Assignment3_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
write_csv(d, "Ass3_new.csv")
```
