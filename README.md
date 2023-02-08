# Fish Parasite Project
Created on 06-02-2023

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#key-file-description">Key File Description</a></li>
	<li><a href="#statistical-hypotheses">Statistical Hypotheses</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>


<!-- ABOUT THE PROJECT -->
## About The Project

![photo](https://user-images.githubusercontent.com/124327996/216885394-6af47385-65ec-425b-98a1-cad19f8a065f.jpg)

This project looks at the effect of internal parasites on the fish host personality, the pumpkinseed sunfish.
Data were collected between May-September 2022 at la Station de Biologie des Laurentides, Université de Montréal, Canada.

The objective is to determine whether an experimental co-infection caused by trematodes (*Uvulifer ambloplitis* and *Apophallus sp*)
and cestodes (*Proteocephalus ambloplitis*) will change the host’s personality (*Lepomis gibbosus*). We looked at exploration, boldness 
and activity before and after an experimental infection in a semi-natural environment (caging experiment in lake). 

This project will generate complex datasets (i.e. repeated measures of behavioral traits through time, responses following a perturbation). 
This type of data structure could apply to researchers working on a wide range of physiological, morphological or behavioural traits.

### Key File Description

1. **Data_raw folder** contains 2 files:  
a) **all_data.csv** : all the raw data collected for the Fish Parasite Project  
b) **meta.data.csv** : all the variables explained with unit of measurement  

2. **R folder** contains:  
a) **fish_parasite.Rproj**, which is the R project  
b) **fish_parasite.R**, which is the script used in the project  

### Biological Hypotheses

Analysis Step 1:

Question 1: Do we find evidence of personality (i.e., consistent differences in behaviour among individuals)? Does boldness, exploration and activity form a beavioural syndrome?

Question 2: Does parasitic infection impact the repeatability of each trait and the strength of behavioural syndromes?

+ Hypothesis 1: We expect that the repeatability of the traits to be reduced because parasites should reduce between individual variance to maximize transmission. Behavioural syndromes should be un-affected. 

+ Hypothesis 2: We expect that the repeatability of the traits to increase because different parasite and host genotypes / immune systems will result in some parasites being more or less affected which should increase between individual variance. Behavioural syndromes might be expected to become stronger (more correlated). 

Analysis Step 2: 

Question 2: How does behaviour change with parasite infection?  

Hypothesis 1: Parasites increase boldness, exploration, and activity because fish are intermediate hosts and increased boldness, activity etc results in a higher probability of predation which facilitates parasite host transmission.  
    
+ Prediction 1(H1): We expect mean boldness, exploration and activity to increase in indiviudals experimentally infected with parasites.  

Hypothesis 2: Parasites decrease boldness, exploration, and activity because it's energetically costly to mount an immune response against parasite infection.  
    
+ Prediction 1(H1): We expect mean boldness, exploration and activity to decrease in indiviudals experimentally infected with parasites.  

### Statistical hypotheses

Analysis Step 1:

**Behavioural syndrome**  
H0: traits are not correlated  
H1: traits are positively correlated (which is often seen with these three traits)  

**Repeatability**  
H0: traits remain constant before and after infection (facilitates predictability of behaviors for future infection)   
H1: traits do not remain constant before and after infection (infection changes trait constancy, making fish more vulnerable)  

Analysis Step 2: 

**Exploration**  
H0: experimental infection has no effect on individuals' exploration  
H1: experimental infection increases exploration (parasitic manipulation, more risk of predation)   
H2: experimental infection decreases exploration (pathological response, amorphous)  

**Boldness**  
H0: experimental infection has no effect on boldness  
H1: experimental infection increases fish boldness (parasitic manipulation to transmit the parasite, thus more risk of predation by increasing boldness)  
H2: experimental infection decreases fish boldness (disease behaviour that makes the fish more cautious for survival)  

**Activity**  
H0: experimental infection has no effect on fish activity  
H1: experimental infection increases activity (to counteract weakening by infection and seek more resources)  
H2: experimental infection decreases activity (since fish are weakened by parasites)  


## Models:

**Three-step Strategy**:

*Step 1*
+ Using all data (60 fish and 4 measurements / fish) we will fit the following models:

1) Model 1: [B, E, A] = u + trtment_{E} + tank + (-1 + trtment_{E}| ID) + (1|Cage)
2) Model 2: [B, E, A] = u + trtment_{E} + (-1 + trtment_{E}| ID) + (1|Cage)

+ Above models allow us to 1) estimate repeatability for ALL traits; 2) estimate the behavioual trait correlations; 3) estimate these within EACH treatment group (C vs E).

*Step 2*
+ Subset the experimental and control fish into two datasets (60 fish and 3 measurements for each C and E group) then fit the following models:


1) Model 1 (Experimental Group): [B, E, A] = u + z_body_condition + z_parasite_load + tank + (1 | ID) + + (1|Cage)
2) Model 2 (Experimental Group): [B, E, A] = u + z_body_condition + z_parasite_load + z_parasite_load^2 + tank + (1 | ID) + + (1|Cage)
3) Model 3 (Control Group): [B, E, A] = u + z_body_condition + tank + (1 | ID) + (1|Cage)

*Step 3*
+ We have two different parasites in E group. So, we want to fit models that possibly look at interactive effects of the two, but this will depend on how many parameters that we need to estimate above.


<!-- GETTING STARTED -->
## Getting Started

How to set the project...

### Prerequisites

Softwares needed and how to install them
* ggplot2
  ```sh
  install.packages("ggplot2")
  ```

<!-- USAGE EXAMPLES -->
## Usage

Examples

<!-- CONTACT -->
## Contact

Maryane Gradito - [@mary_gradito](https://twitter.com/mary_gradito) - email: maryane.gradito@umontreal.ca

Project Link: [https://github.com/MaryaneGradito/fish_parasite](https://github.com/MaryaneGradito/fish_parasite)

<p align="right">(<a href="#readme-top">back to top</a>)</p>
<
