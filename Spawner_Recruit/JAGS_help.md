### Running Bayesian SR model 
Bayesian SR Analysis model is coded in **JAGS** (see Model Codes Tab). SR models included are **Ricker** and **Beverton-Holt** with options of **AR(1) Error** and **Time varying alpha (TVA)**  

---
**Model Running Steps**    
* **Select SR Model :**   Ricker or Beverton-Holt
* **Model Addition :**  None (Standard), AR(1) Error, or Time varying alpha 
* **Set Simulation:** Use default numbers or increase numbers
* **Click Run** Wait until Trace plots and Summary show up.  After that, click other tabs. 

---
**Bayesian Model Setting**  
Bayesian analyses is based on numerical simulation and sampling. Bayesian statistical quantities (e.g., model parameters estimates, CI) are simple summary of samples from the simulation. To obtain good statistics, samples needs to be taken from the simulation should be stable.  When the simulation reaches stable state, trace plots should look flat bands, and density plots should have a single defined peak.  **Note** Because Bayesian model is based on simulation, estimated parameter values will differ every time model is run. 


To achieve the stable simulation, every Bayesian models have following 4 controls:
* **Burn-in**  Initial simulation and highly unstable, so that all samples in this stage is thrown out. (Default: 1000)  
* **Simulation**  This is the stage when samples can be taken. Ideally this phase should be stable. (Default: 10000) 
* **Thinning**  Sample every xxth simulation. Length of the simulaiton and thinning will determine the number of samples from which parameter estimates are calculated: samples = simulation/ thinning.   Generally, this should be from 1000 to 10000 (Default 10)
* **Chains**  The number of simulation experiments with different starting points. JAGS selects starting points randomly from the priors. If model is correct, final simulation should reach identical mean-median regardless starting points. The number of chains are generally 1 - 5. (Default 1)  


Under the default settings, a total of (burn-in + simulation)xchains (1000+10000)x1 =11000) simulation is conducted.  Of those, 1000 samples are taken (simulation/thinning = 10000/10 = 1000).  Model parameters estimates are based on 1000 samples.  
The default is set to produce **quick and reasonable estimates**. It is **recommended** to increase the length of burn-in and simulations and the number of chains to obtain **better and more stable estimates**.  Generally, **longer burn-in and simulation and moree chains will produce better model parameter estimates.** However, this will also **increase** simulation time significantly. A rule of thumb is (1) run the model with default setting, (2) check trace and density plots, (3) if the plots do not look good, increase burn-in, simulation, and chains until getting good plots.   
When good parameter estimates are not achieved after long simulations, this is an indication that: (1) **data are not informative**, or (2) **wrong model specifications**.  


## Spawner-Recruit model 

Two Spawner-Recruit models are available:**Ricker** and **Beverton-Holt** 

### Ricker model 
$$R = \alpha Se^{-\beta S}e^{\varepsilon}$$

### Beverton-Holt model 
$$R = \frac{\alpha S}{1+\beta S}e^{\varepsilon}$$

where 
$$\varepsilon_{iid} \sim N(0,\sigma ^{2})$$

In JAGS model,basic form of the model fitting is 

$$ln(`R) \sim N(\mu,{\sigma}^{2})$$
where $\mu$ is Ricker or Beverton-Holt Spawner Recruit equations.


Log linearlized form of Ricker and Beverton-Holt models are  

**Ricker Model**
$$ln(`R)= ln(\alpha )-\beta s+ln(S)+\varepsilon$$

**Beverton-Holt Model**
$$ln(`R)=ln(\alpha )-ln(1+\beta s)+ln(S)+e$$
where
$$s = S\cdot{10}^{-d}$$
and  
$$ d = Int(log(S)) $$

This modeling formulation ensures that $ln(\alpha)$ and $\beta$ be at the same number rage. This makes it easier to estimate model parameters. 

#### AR(1) model option 
AR(1) model (First-order autoregression model) assumes that error is serially correlated or that the value of residual at time *t* is a linear function of the residual at time *t-1*. 
This is modeled as 
$${\varepsilon_{t}}=\phi {\varepsilon_{t-1}}+{\nu_{t}}$$ 
where ${\nu_{t}} \sim N(0,{\sigma _{\nu }}^{2})$

#### Time variant alpha model option  
Time variant alpha model assumes that alpha takes random walk process. 
$${ln(\alpha )_{t}}={ln(\alpha )_{t-1}}+{\omega_{t}}$$ 
where ${\omega_{t}} \sim N(0,{\sigma _{\omega }}^{2})$

#### Model Parameters and Priors 
The model estimates following parameters

|Parameter |  | Prior  |  |    Note  |
|  ------- |---:| -------:|---:|-------:|
|_________|_|________________|_|________________________|
| $ln(\alpha)$  | | $ln(\alpha) \sim U(0,10)$ | | All models   | 
| $\beta$    |  |   $\beta \sim U(0,10)$  | | All models   | 
| $\sigma$    |   |   $\sigma \sim U(0,10)$  | | All models    | 
| $\phi$    |   | $\phi \sim U(-1,1)$   |  |**AR1 option**  | 
| $\varepsilon_{0}$ |   |  $\varepsilon_{0} \sim N(0,100)$   |  |**AR1 option**  | 
| $\sigma _{\omega}$|   |  $\sigma _{\omega} \sim U(0,10)$   |  |**TVA option**| 

---
All the priors are set to be flat, uninformative, reasonable range that are seen in salmon stock assessment. Model starting points are randomly selected from the above prior ragnes. 

---
#### Biological reference points
Following biological reference points are generated from the model 
* Seq   Spawner abundance expected to produce the same number of recruits
* Smsy  Spawner abundance espected to produce the maximum number of yield (MSY) * Umsy  Exploitation rate exepected to produce MSY
* Smax  Spawner abundance expected to produce the maximum number of recruits

**Ricker model** 
$$Seq =  \frac{ln(\alpha)}{\beta}$$
Smsy and Umsy of Riker model is an approximate where 0 < $ln(\alpha)$ < 3(Hilbon 1985) 
$$Smsy =  Seq(0.5-0.07ln(\alpha))$$
$$Umsy =  ln(\alpha)(0.5-0.07ln(\alpha))$$
$$Smax =  \frac{1}{\beta}$$
**Beverton-Holt model** 
$$Seq =  \frac{\alpha -1}{\beta}$$
$$Smsy =  \frac{\sqrt{\alpha} -1}{\beta}$$
$$Umsy = 1- \sqrt{\frac{1}{\alpha}}$$
Smax does bit exist in Beverton-Holt model
$$Smax = NA$$

### Bayesian SR model prediction 
After the model fitting.   




