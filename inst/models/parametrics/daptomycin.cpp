[ PROB ]

Garreau, R., Bricca, R., Gagnieu, M. C., Roux, S., Conrad, A., Bourguignon, L., Ferry, T., Goutelle, S., & Lyon Bone and Joint Infection Study Group (2021).
Population pharmacokinetics of daptomycin in patients with bone and joint infection: minimal effect of rifampicin co-administration and confirmation of a sex difference.
The Journal of antimicrobial chemotherapy, 76(5), 1250–1257. 
 
https://doi.org/10.1093/jac/dkab006

[ PARAM ] @annotated
TVCL : 0.365 : Clearance
TVV1 : 3.59  : Central volume
TVV2 : 4.71  : Peripheral volume of distribution
TVQ  : 0.752 : Intercompartmental clearance

ETA1: 0 : Clearance (L/h)
ETA2: 0 : Central volume (L)
ETA3: 0 : peripheral volume (L)
ETA4: 0 : intercompartmental clearance (L/h)

[ PARAM ] @annotated @covariates
TBW  : 79.2  : Total body weight mean value
CRCL : 109   : Creatinine clearance (mL.min-1)
AGE  : 60.4  : Age (years)
SEX  : 0     : Sex 0 = Female, 1 = Male
RIF  : 0     : Patient have concomitent treatment with rifampicin = 1. RIF = 0

[ THETA ] @annotated
0.43   : CRCL on CL
0.232  : SEX on CL 
0.603  : TBW on V1
0.263  : Age on V1
0.117  : SEX On V
-0.121 : RIF on V


[ OMEGA ] @annotated @correlation @block
ETACL : 0.196 : Omega Clearance (L/h)
ETAV1 : 0.515 0.123 : Omega Central volume (L)
ETAV2 : 0.000 0.000 0.290 : Omega V2
ETAQ  : 0.000 0.000 0.000 1.13  : Omega Q

[ SIGMA ] 
0.053 // prop
2.38  // additive

[ CMT ] @annotated
CENT  : Central compartment (mg/L)[ADM, OBS]
PERIPH: Peripheral compartment ()

[ MAIN ]
double CL_CRCL = THETA1;
double SEX_CL  = THETA2;
double TBW_V1  = THETA3;
double AGE_V1  = THETA4;
double SEX_V1  = THETA5;
double RIF_V1  = THETA6;

double CL = TVCL * exp(ETA1 + ETA(1)) * exp(CL_CRCL * log((CRCL/109)) + pow(SEX, SEX_CL));
double V1 = TVV1 * exp(ETA2 + ETA(2));
double Q = TVQ * exp(ETA4 + ETA(4));
double V2 = TVV2 * exp(ETA3 + ETA(3));

double K12 = Q / V1  ;
double K21 = Q / V2  ;
double K10 = CL / V1 ;

[ TABLE ]
double DV = (CENT/V1); // * sqrt(pow((1 + EPS(1)), 2) + pow(EPS(2), 2));

$ODE
dxdt_CENT   =  K21 * PERIPH - (K10 + K12) * CENT ;
dxdt_PERIPH =  K12 * CENT - K21 * PERIPH ;

[ CAPTURE ] DV CL V1