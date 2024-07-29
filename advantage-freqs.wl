Get["cryptolib.ma"]

CoincidenceIndex[testo_, trunc_, n_] :=
 If[
  StringQ[testo],
  (*THEN*)
  CoincidenceIndex[TextCode[testo], trunc, n],
  (*ELSE*)
  Module[{ntesto, tfreqs},
   (
    ntesto = Take[testo, n];
    tfreqs = Take[Sort[Map[Count[ntesto, #] &, Range[0, 25]]], -trunc];
    N[Plus @@ (tfreqs (tfreqs - 1)/(n (n - 1)))]
    )]
  ];



testo = StringDrop[Import["testo.txt"], 146];
random = Table[RandomInteger[25], {2500}];

CoincidenceIndex[testo]
plaintext = TextCode[testo];
(*Plus @@ (Map[First, DistributionA[plaintext]]^2);*)

ita = Import["http://www.corriere.it"];
divina = 
  "nelmezzodelcammindinostravitamiritrovaiperunaselvaoscuracheladiritt\
aviaerasmarritaahiquantoadirqualeraecosaduraestaselvaselvaggiaeaspraef\
ortechenelpensierrinovalapauratanteamarachepocoepiumortemapertrattarde\
lbenchivitrovaidirodelaltrecosechivhoscorteiononsobenridircomivintrait\
anterapiendisonnoaquelpuntochelaveraceviaabbandonaimapoichifuialpiedun\
collegiuntoladoveterminavaquellavallechemaveadipaurailcorcompuntoguard\
aiinaltoevidilesuespallevestitegiaderaggidelpianetachemenadrittoaltrui\
perognecalleallorfulapauraunpocoquetachenellagodelcormeraduratalanotte\
chipassaicontantapietaecomequeicheconlenaaffannatauscitofuordelpelagoa\
larivasivolgealacquaperigliosaeguatacosilanimomiochancorfuggivasivolse\
aretroarimirarlopassochenonlasciogiamaipersonavivapoicheiposatounpocoi\
lcorpolassoripresiviaperlapiaggiadisertasichelpiefermosempreeralpiubas\
soedeccoquasialcominciardelertaunalonzaleggieraeprestamoltochedipelmac\
olatoeracovertaenonmisipartiadinanzialvoltoanzimpedivatantoilmiocammin\
ochifuiperritornarpiuvoltevoltotemperadalprincipiodelmattinoelsolmonta\
vansuconquellestellecheranconluiquandolamordivinomossediprimaquellecos\
ebellesichabenesperarmeracagionediquellafieraalagaettapelleloradeltemp\
oeladolcestagionemanonsichepauranonmidesselavistachemapparvedunleonequ\
estipareachecontramevenisseconlatestaltaeconrabbiosafamesichepareachel\
aerenetremesseedunalupachedituttebramesembiavacarcanelasuamagrezzaemol\
tegentifegiavivergramequestamiporsetantodigravezzaconlapaurachusciadis\
uavistachioperdeilasperanzadelaltezzaequalequeichevolontieriacquistaeg\
iugneltempocheperderlofacechentuttisuoipensierpiangeesattristatalmifec\
elabestiasanzapacechevenendomincontroapocoapocomiripignevaladovelsolta\
cementrechirovinavainbassolocodinanzialiocchimisifuoffertochiperlungos\
ilenziopareafiocoquandovidicostuinelgrandisertomisereredimegridaialuiq\
ualchetusiiodombraodomocertorispuoseminonomoomogiafuieliparentimieifur\
onlombardimantoaniperpatriaambeduinacquisubiulioancorchefossetardievis\
siaromasottolbuonoaugustoneltempodelideifalsiebugiardipoetafuiecantaid\
iquelgiustofigliuoldanchisechevenneditroiapoichelsuperboilionfucombust\
omatupercheritorniatantanoiaperchenonsaliildilettosomontecheprincipioe\
cagiondituttagioiaorsetuquelvirgilioequellafontechespandidiparlarsilar\
gofiumerispuosioluiconvergognosafronteodelialtripoetionoreelumevagliam\
illungostudioelgrandeamorechemhafattocercarlotuovolumetuselomiomaestro\
elmioautoretusesolocoluidacuiotolsilobellostilochemhafattoonorevedilab\
estiapercuiomivolsiaiutamidaleifamososaggiochellamifatremarleveneeipol\
siateconvientenerealtroviaggiorispuosepoichelagrimarmividesevuocampard\
estolocoselvaggiochequestabestiaperlaqualtugridenonlasciaaltruipassarp\
erlasuaviamatantolompediscecheluccideehanaturasimalvagiaeriachemainone\
mpielabramosavogliaedopolpastohapiufamechepriamoltisonlianimaliacuisam\
mogliaepiusarannoancorainfinchelveltroverrachelafaramorircondogliaques\
tinonciberaterranepeltromasapienzaamoreevirtuteesuanazionsaratrafeltro\
efeltrodiquellaumileitaliafiasalutepercuimorilaverginecammillaeurialoe\
turnoenisodiferutequestilacacceraperognevillafinchelavrarimessanelonfe\
rnolaondenvidiaprimadipartillaondioperlotuomepensoediscernochetumisegu\
ieiosarotuaguidaetrarrottidiquiperlocoetternooveudirailedisperatestrid\
avedrailiantichispiritidolentichalasecondamorteciascungridaevederaicol\
orchesoncontentinelfocoperchesperandivenirequandochesiaalebeategential\
equaipoisetuvorraisalireanimafiaaciopiudimedegnaconleitilasceronelmiop\
artirechequelloimperadorchelasuregnaperchifuribellantealasualeggenonvu\
olchensuacittapermesivegnaintuttepartiimperaequivireggequivielasuacitt\
aelaltoseggioohfelicecoluicuivieleggeeioaluipoetaiotiricheggioperquell\
odiochetunonconoscestiaciochiofuggaquestomaleepeggiochetumimeniladovor\
dicestisichioveggialaportadisanpietroecolorcuitufaicotantomestiallorsi\
mosseeiolitennidietro";

(* uso la distribuzione del testo per autocorrelare la parte estratta \
nel cifrato*)

refita = TextCode@divina;
mita = CoincidenceIndex[refita];
Print["riferimento italiano : ",mita, DistributionA[refita]]


key = TextCode["spada"]
Print["Statistical Characteristics of plaintext blocks :"];

Map[DistributionA, blocks = Transpose@Partition[plaintext, m]]

ciphertext = Flatten@Transpose@Mod[blocks + key, 26]; FromCode@ciphertext;

m = Length@key;

Print["Statistical Characteristics of plaintext blocks :"];
Map[DistributionA, blocks = Transpose@Partition[plaintext, m]]

Print["compare coincidence index of reference : ",CoincidenceIndex[refita], 
      "with thw coincidence index of plaintext : "CoincidenceIndex[plaintext]];

Print["Statistical Characteristics of ciphertext blocks :",
MutualInformation[ciphertext, refita]
Map[CoincidenceIndex, blocks = Transpose@Partition[ciphertext, m]]
];


Map[DistributionA, blocks]
Map[MutualInformation[#, refita] &, blocks]
Histogram[{ciphertext, refita}, 26, "Probability"]

G = ListLinePlot[
   Transpose@
    Table[Map[CoincidenceIndex[#, 10, len] &, 
      Prepend[blocks, random]], {len, 10, 200}], PlotRange -> All];

wblocks = Transpose@Partition[ciphertext, m - 1];
Gwrong = 
  ListLinePlot[
   Transpose@
    Table[Map[CoincidenceIndex[#, 10, len] &, 
      Prepend[wblocks, random]], {len, 10, 200}], 
   PlotStyle -> {Thick, Opacity[0.1]}, PlotRange -> All];


Show[G, Gwrong]