;; 1. Based on:  
;; 2. Description: 2 cmt model, 1st order abs, BSV on CL/F and Ka
;; 3. Author: Helena Edlund
;; 4. Date:  
;; 5. Version:  
;; 6. Label:  
;; 7. Structural model:  
;; 8. Covariate model:  
;; 9. Inter-individual variability:  
;; 10. Inter-occasion variability:  
;; 11. Residual variability 
;; 12. Estimation:  

$PROBLEM   

$SUB ADVAN4 TRANS4

$INPUT C NMSEQSID=ID TIME TAPD DV AMT EVID CMT OCC BLQ DOSE ADDL II BWT AGE BCRCL SEXM RACE STUDYID DAY

$DATA simData.csv
IGNORE=@

$PK

TVKA = THETA(1)
KA = TVKA*EXP(ETA(1))

TVCL = THETA(2)
CL = TVCL*((BWT/70)**0.75)*EXP(ETA(2))

TVV2 = THETA(3)
V2 = TVV2 * ((BWT/70)**1) * EXP(ETA(3))

Q = THETA(4)
V3 = THETA(5)

S2 = V2

$THETA
(0, 1.8)  ; KA ;h-1;
(0, 11)    ; CL ;L/h;
(0, 70)    ; V2 ;L;
(0, 20)    ; Q ;L/h;
(0, 210)    ; V3 ;L;

$OMEGA 0.06   ; BSV_KA     
$OMEGA BLOCK(2) 0.08 0.0016 0.04

$SIGMA
0.03  ; PROP

$ERROR
IPRED=F
Y = IPRED*(1+EPS(1))

$SIM (181127321) ONLYSIMULATION SUBPROBLEM=1 

$TABLE C NMSEQSID TIME TAPD DV AMT EVID CMT OCC BLQ DOSE ADDL II BWT AGE BCRCL SEXM RACE STUDYID DAY NOAPPEND ONEHEADER NOPRINT FILE=simtab001
