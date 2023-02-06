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

### Statistical hypotheses

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

**Behavioural syndrome**  
H0: traits are not correlated  
H1: traits are positively correlated (which is often seen with these three traits)  

**Repeatability**  
H0: traits remain constant before and after infection (facilitates predictability of behaviors for future infection)   
H1: traits do not remain constant before and after infection (infection changes trait constancy, making fish more vulnerable)  

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
