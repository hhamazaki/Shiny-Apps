
## Spawner-Recruit model 

Two Spwner-Recruit models are available:**Ricker** and **Beverton-Holt** with additonal options of **AR(1) error** and **Time variant alpha**
After slecting SR model click **AR(1) Error**,or **Time variying alpha** radiobutton to activate this option. 

### Ricker model 
$$R = \alpha Se^{-\beta S}e^{\varepsilon}$$
Linearlized form of Ricker SR model is
$$ln(`R)= ln(\alpha )-\beta S+ln(S)+\varepsilon$$
### Beverton-Holt model 
$$R = \frac{\alpha S}{1+\beta S}e^{\varepsilon}$$Linearlized form of Beverton-Holt SR model is
$$ln('R)=ln(\alpha )-ln(1+\beta S)+ln(S)+e$$
where 
$$\varepsilon_{iid} \sim N(0,\sigma ^{2})$$
#### AR(1) model option 
AR(1) model (First-order autoregresssion model) assumes that error is serially correlated or that the value of residual at time *t* is a linear function of the residual at tune *t-1*. 
This is modeled as 
  $${\varepsilon_{t}}=\phi {\varepsilon_{t-1}}+\nu$$ 
where ${\nu_{iid}} \sim N(0,{\sigma _{\nu }}^{2})$

### Time variant alpha model option  
Time variant alpha model assumes that alpha takes random walk process. 
$${ln(\alpha )_{t}}={ln(\alpha )_{t-1}}+{\omega_{t}}$$ 
where ${\omega_{t,iid}} \sim N(0,{\sigma _{\omega }}^{2})$


#### Biological reference points
Following biological reference points are generated from the model 
* Seq   Spawner abundance expected to produce the same number of recruits
* Smsy  Spawner abundance espected to produce the maximum number of yield (MSY) 
* Umsy  Exploitation rate exepected to produce MSY
* Smax  Spawner abundance expected to produce the maximum number of recruits

##### Seq
SEQ is also the point where spawner-recruit curve intercepts 1:1 (replacement) line. 

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
Smax does not exist in Beverton-Holt model
$$Smax = NA$$
## Running Bayesian SR model 
Bayesian SR Analysis model is coded in **JAGS** (see Model Codes Tab). 

$$ln(`R) \sim N(\mu,{\sigma}^{2})$$

where $\mu$ is the equations above. 








