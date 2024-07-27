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


[ PKMODEL ] cmt = "CENT, PERIPH"

[ OMEGA ] @annotated
ETACL : 0.196 : Omega Clearance (L/h)
ETAV1 : 0.123 : Omega Central volume (L)
ETAV2 : 0.29  : Omega V2
ETAQ  : 1.13  : Omega Q

[ MAIN ]

double CL_CRCL = THETA1;
double SEX_CL  = THETA2;
double TBW_V1  = THETA3;
double AGE_V1  = THETA4;
double SEX_V1  = THETA5;
double RIF_V1  = THETA6;

double LOG_CL = log(TVCL) + CL_CRCL * (CRCL/109) + pow(SEX, SEX_CL);
double CL = exp(LOG_CL + ETA(1));

double LOG_V1 = log(TVV1) + AGE_V1 * (AGE/60.4) + TBW_V1 * (TBW/79.2) + pow(RIF, RIF_V1)+ pow(SEX, SEX_CL);
double V1 = exp(LOG_V1 + ETA(2));

double Q = exp(log(TVQ) + ETA(3));
double V2 = exp(log(TVV2) +ETA(4));


[ SIGMA ] @annotated
EP : 0.05 : not used proportional

[ TABLE ]
double CC = (CENT/V1);

[ CAPTURE ] CC