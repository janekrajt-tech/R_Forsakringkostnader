## Försäkringskostnader – Analys Beskrivning

Projektet analyserar vilka faktorer som påverkar försäkringskostnader
(charges) med hjälp av ett dataset innehållande variabler som ålder,
BMI, rökning och hälsa.

## Frågeställningar 

Vilka variabler påverkar kostnader mest?

Finns skillnader mellan grupper (t.ex. rökare vs icke-rökare)? 

Hur väl kan en regressionsmodell förklara kostnader?

## Struktur

scripts/

01_load_data.R – läser in och undersöker data

02_data_clean.R – städar data och skapar variabler

03_analysis.R – visualiseringar och beskrivande analys

04_model.R – regressionsmodell

## Körning

Alla scripts finns samlade på run_analys.R

source("scripts/01_load_data.R")

source("scripts/02_data_clean.R")

source("scripts/03_analysis.R")

source("scripts/04_model.R")

## Output

Grafer, tabeller och regressionsresultat som visar samband mellan
variabler och kostnader.

## Paket

Tidyverse
