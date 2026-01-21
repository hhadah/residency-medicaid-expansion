# Medicaid Expansion and the Number of Residency Spots  

This is a project with JR Ang to study the effect of the artificially imposed cap on the number of residency spots in the US by the AMA. This is project will lead to several papers studying the effect of number of doctors on health outcomes, healthcare costs, healthcare access, healthcare quality, healthcare disparities, and other related topics. We will also study the effect of Medicaid expansion on the number of residency spots, and with it the number of doctors in the US. Consequently, we will study the effect of Medicaid expansion on the above health outcomes. 

My to-do list for this project is as follows:
- [X] Clean and process the data on residency spots
- [X] Reshape the data to a panel format
- [X] Run DD analysis using Borusyak, Jaravel, and Spiess (2024) leveraging variation in timing of Medicaid expansion across states on the number of residency spots at the institution/hospital level
- [X] Run DD analysis using Borusyak, Jaravel, and Spiess (2024) leveraging variation in timing of Medicaid expansion across states on the number of filled residency spots at the institution/hospital level
- [X] Run the DD analysis by specialty
- [ ] Run DDD analysis on the above regressions by different county characteristics (e.g. rural vs urban, high income vs low income, etc)

## Notes from SEA Conference Feedback

### 1. FMAP and Medicaid GME Matching
* **Temporal Analysis:** Explore how **FMAP matching rates** have shifted over time, particularly in light of recent federal regulatory changes.
* **Funding Capacity:** Since Medicaid GME payments are FMAP-matchable, variations in these rates may directly impact a state's capacity or willingness to fund residency programs.

### 2. Hospital-Level Weights
* **Alternative Schemes:** Test different weighting schemes in hospital-level regressions to see if they resolve the divergence between state-level increases and hospital-level decreases.
* **Program Size:** Investigate whether the observed patterns are driven primarily by **large teaching hospitals** versus smaller programs.

### 3. HPSA Designations
* **Shortage Areas:** Examine how **Health Professional Shortage Area (HPSA)** status influences changes in residency slots.
* **Heterogeneity:** HPSA hospitals often face unique staffing incentives or Medicare funding rules, providing a useful source of variation for Medicaid expansion responses.

### 4. Clarify the Funding Framework
* **Conceptual Breakdown:** Provide a clear map of funding sources, including:
    * Medicaid
    * Medicare
    * Internal Hospital Funds
* **Mechanism:** Explicitly link this framework to the observed changes in slots to help the reader understand the underlying financial drivers.

### 5. Outcome Definition
* **Normalization vs. Raw Data:** Re-evaluate the use of "slots per 100,000 population" at the hospital level. 
* **Logic:** Since hospitals rarely make internal decisions based on regional population density, consider switching to **raw slot counts** for these specific analyses.

### 6. Trace Funding Sources
* **Mapping:** More clearly delineate the specific funding streams for residency slots.
* **Causal Story:** Strengthening this mapping will help link state-level Medicaid policy changes directly to individual hospital decisions.

### 7. Include Time-Varying Controls
* **Model Refinement:** Incorporate relevant time-varying controls into the empirical model.
* **Omitted Variables:** This will help isolate the **causal effect** more cleanly and reduce concerns regarding omitted-variable bias.