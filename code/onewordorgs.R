
#list of one word org name
orgs <- str_spl(
c("

AARP
AAUW
ACD
ACM
Acrisure
ACS
ACUITY
Acushnetompany
Aetna
AgJobs
AMAN FARMS
AFSCME
API
Apple
A.O.I.B.P.
ADP
Airgas
AKziom
ALK
AMPS
AMV
Anon
APF
APS
ArcelorMittal
Arch
ASCE
ASEI
Astronics
APWU
AWEE
AYE
BNI
BHHS
biofuelwatch
care
Catalyst
BTS
BAE
BAHRA
Baxter
BCBST
BCSB
Beeson
BNA
brianleesblog
Bridgestone
Brownells
BSi
CANOY
CERES
Chevron
C
Cablevision
CAPC
Caterpillar
cchf
CCHF
CCI
CCM
celestialreliefmassage
ChemTreat
Chevron
Choa
Chromalox
CHS
Cigna
Cinematiceye
CLSMF
CMI
CNCPROSE
Comcast
ComCor
CommScope
Computers
Concordia
Confidential
ConocoPhillips
Conservice
DICKS
Domtar
CPA
CreativeBizSolutionsbyCBoudreaux
CRL
Cryovac
CVC
CW
D
Daktronics
DAKTRONICS
Davis
DEMCO
DHHS
DLA
DoD
DofA
DOL
DRC
DTAG
DTD
earthjustice
earthworks
emblemHealth
EAW
EcoRigs
ECS
ECVA
EDMC
EduKit
egHRConsulting
Elevanta
EmCentrix
Emerson
Enersys
EnovateIT
ESM
Evenflo
EVOA
Exterran
Fiberweb
Fibrek
Fiducial
FMCC
Freedomworks
FreedomWorks
FSCSC
FWNA
FXI
G. Boren Services
^GD$
G.O.A.
GEOA
Gerdau
GFHS
GFY
GlaxoSmithKline
Goodrich
^gop$
Governor
GreenbergFarrow
GSH
GSK
GTRI
HayssenSandiacre
HCA
FORGE
greenpeace
G.W. Restaurant Group
H. E. A. R.T.
J. King's Restaurant
J.C. Wilson Associates
M.E.E.R. e.V.
HDRadio
Hilton
HISC
HomeAlone
Honeywell
HR
HRMA
HRPBC
HRx
HSBC
HSET
HTA
HTSI
HMS
Humana
Homegrown
ibew
IUPUI
I.A.M. National Pension Fund
IAC
IBM
IEC
ieh
IG
IHG
Illuminex
ILO
IMB
Infilaw
Innovet
InSinkErator
INTERNETworks
IOMMP
sis
ITC
ITD
ITI
J.D. Levesque & Associates
Jennmar
J. E. McLamb and Sons
JHSMH
jpeg
KBR
KCC
KFC
KEMA
KFS
LAUSD
LMHC
LULAC
microsoft
motionfirst
^MoveOn
KBR
KCC
KEMA
KFS
Komyo
LAB
Labot
LeadingAge
LeanBizSolutions
LECET
Lennox
LFH
Lifespan
LIUNA
LMSO
LVI
LWCC
MACNY
MACU
Manpower
MARTA
Mattel
MAU
McLane
MCNC
MCS
Medela
Mediware
MEMIC
Mercer
MGIC
^NAR$
N.C. State Grange
^NAP$
MOBIS
ModusLink
Monsanto
Moxtek
MTN
Multisorb
MVN
NAACP
N.R.A
NBC
NEC
NEMI
NFI
NFIB
NGC
NHC
NHS
NIHRA
NJVC
NLRB
NMC
NASWA
NCPPA
NYSUT
Nordstrom
Nordyne
Novation
NOYFB
NSBA
NYB
oceana
offshore systems
oneDigital
Oakleym
OCMMC
OfficeMax
Omni
Omnidian
ORAU
OREC
PDA
pennenvironment
OrthoIndy
Osborn
OWGA
Oxy
PAIRE
PAL
Paychex
PeaceHealth
Peopleopolis
PHRA
PMC
PNC
PNS
PointBank
PPAI
Prayon
Pregis
President
Primerica
PrimeSource
Printpack
ProduceOne
Proprietor
prudential
qps
qss
quill
R.F. Products
Redstate
Reganomic
Republican
ret
S. C. Davee Associates
S.D.Estes Consulting
Safariland
SAFCU
SAG
SCA
SCH
SCIHRA
Sears
sef
SEIU
Seld
Senate
SHW
SIOP
SirsiDynix
Skanska
Sodexo
Software
SPCA
SSFCU
Streamlite
Suddenlink
Supplemental
SURG
Symbeo
Synergy
TAHRA
Tanamera
TCRP
TDECU
Teamsters
TEAPARTY
Technicolor
Tegrant
Teichert
TelAlaska
Textron
TFCU
THARCO
TheCarolinaCowboy
ThedaCare
TimBar
TIMET
TJC
Toyota
TransCore
TransMontaigne
TRC
TruStone
TSYS
TTCI
TWIA
Tyco
regence
R. F. Lewis & Sons
SHRM
SkyTruth
Transamerica
TriNet
ToolTech
USCCB
USDA
r.e.m films
S.E.Alaska Outdoor Adventures
U.S. Army Installation Management Command
U.S. Army Installation Management Command
U.S. Army NAF
U.S. House Committee on Education and the Workforce
U.S. Labs
U.S. Military
U.S. House of Representatives
U.S. Senate
U.S. Army Installation Management Command
U.S. Army NAF
U.S. Coast Guard
U.S. Department of Interior
U.S. EPA Region 9
U.S. Fish and Wildlife Service
U.S. House of Representatives
U.S. Marine Corporation
U.S. National Park Service
U.S. Navy
UB
UCH
UEP
UMWA
UNFI
UPS
URS
USG
USMC
USPIS
USPS
USTS
UTC
UW
UWUA
Valero
Valspar
Vandover
Verizon
vfw
VIA
Vigilant
Village
VisitPittsburgh
Weinberg
Woodbores
WorkingBuildings
Worthington
WVDNR
YESCO
Walmart
waterlegacy
WNEC
WSCHR
WSUSA
WTP
WZF
XeniumHR
YDL
YRMC"), "\n"
) %>%
unlist()

orgsShort <- str_c(orgs, collapse = "|")


#Advantix Systems
#REMAX Select Properties/KGD INC.


