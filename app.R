## ============================================================
##  Biological Clock of Multimorbidity — Shiny App
##  Jon Sánchez-Valle et al., 2025
##
##  Run from inside the ShinyApp/ directory:
##    shiny::runApp("ShinyApp")   or   shiny::runApp()  if already there
##
##  Expected structure:
##    ShinyApp/
##      app.R
##      Data/
##        prevalences.txt
##        age_diagnosis.txt
##        Disease_colors.txt
##        networks/
##          RR_net_{sex}_{w1}_{w2}.txt   (sex: both/men/women)
## ============================================================

library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(plotly)
library(visNetwork)
library(igraph)
library(DT)

set.seed(42)

## ── ICD-10 three-digit names (WHO 2019, ~911 codes) ─────────────────────────
ICD10_NAMES <- c(
  "A00"="Cholera","A01"="Typhoid and paratyphoid fevers",
  "A02"="Other salmonella infections","A03"="Shigellosis",
  "A04"="Other bacterial intestinal infections",
  "A05"="Other bacterial foodborne intoxications","A06"="Amoebiasis",
  "A07"="Other protozoal intestinal diseases",
  "A08"="Viral and other specified intestinal infections",
  "A09"="Diarrhoea and gastroenteritis of presumed infectious origin",
  "A15"="Respiratory tuberculosis","A17"="Tuberculosis of nervous system",
  "A18"="Tuberculosis of other organs","A19"="Miliary tuberculosis",
  "A30"="Leprosy","A36"="Diphtheria","A37"="Whooping cough",
  "A39"="Meningococcal infection","A40"="Streptococcal septicaemia",
  "A41"="Other septicaemia","A46"="Erysipelas","A50"="Congenital syphilis",
  "A51"="Early syphilis","A52"="Late syphilis","A54"="Gonococcal infection",
  "A56"="Other sexually transmitted chlamydial diseases","A59"="Trichomoniasis",
  "A60"="Anogenital herpesviral infection",
  "A63"="Other predominantly sexually transmitted diseases",
  "A64"="Unspecified sexually transmitted disease","A80"="Acute poliomyelitis",
  "A82"="Rabies","A87"="Viral meningitis","A90"="Dengue fever","A95"="Yellow fever",
  "B00"="Herpesviral infections","B01"="Varicella","B02"="Zoster","B05"="Measles",
  "B06"="Rubella","B07"="Viral warts",
  "B08"="Other viral infections with skin and mucous membrane lesions",
  "B15"="Acute hepatitis A","B16"="Acute hepatitis B",
  "B17"="Other acute viral hepatitis","B18"="Chronic viral hepatitis",
  "B19"="Unspecified viral hepatitis","B20"="HIV disease",
  "B25"="Cytomegaloviral disease","B26"="Mumps","B27"="Infectious mononucleosis",
  "B30"="Viral conjunctivitis","B33"="Other viral diseases",
  "B34"="Viral infection of unspecified site","B35"="Dermatophytosis",
  "B36"="Other superficial mycoses","B37"="Candidiasis","B44"="Aspergillosis",
  "B49"="Unspecified mycosis","B50"="Plasmodium falciparum malaria",
  "B54"="Unspecified malaria","B58"="Toxoplasmosis","B65"="Schistosomiasis",
  "B67"="Echinococcosis","B77"="Ascariasis","B85"="Pediculosis and phthiriasis",
  "B86"="Scabies","B90"="Sequelae of tuberculosis",
  "B94"="Sequelae of infectious diseases","B99"="Other infectious diseases",
  "C00"="Malignant neoplasm of lip","C01"="Malignant neoplasm of base of tongue",
  "C02"="Malignant neoplasm of tongue","C03"="Malignant neoplasm of gum",
  "C05"="Malignant neoplasm of palate","C07"="Malignant neoplasm of parotid gland",
  "C09"="Malignant neoplasm of tonsil","C10"="Malignant neoplasm of oropharynx",
  "C11"="Malignant neoplasm of nasopharynx","C13"="Malignant neoplasm of hypopharynx",
  "C15"="Malignant neoplasm of oesophagus","C16"="Malignant neoplasm of stomach",
  "C17"="Malignant neoplasm of small intestine","C18"="Malignant neoplasm of colon",
  "C19"="Malignant neoplasm of rectosigmoid junction","C20"="Malignant neoplasm of rectum",
  "C21"="Malignant neoplasm of anus","C22"="Malignant neoplasm of liver",
  "C23"="Malignant neoplasm of gallbladder","C25"="Malignant neoplasm of pancreas",
  "C32"="Malignant neoplasm of larynx","C33"="Malignant neoplasm of trachea",
  "C34"="Malignant neoplasm of bronchus and lung","C43"="Malignant melanoma of skin",
  "C44"="Other malignant neoplasms of skin","C45"="Mesothelioma",
  "C50"="Malignant neoplasm of breast","C51"="Malignant neoplasm of vulva",
  "C52"="Malignant neoplasm of vagina","C53"="Malignant neoplasm of cervix uteri",
  "C54"="Malignant neoplasm of corpus uteri","C56"="Malignant neoplasm of ovary",
  "C60"="Malignant neoplasm of penis","C61"="Malignant neoplasm of prostate",
  "C62"="Malignant neoplasm of testis","C64"="Malignant neoplasm of kidney",
  "C67"="Malignant neoplasm of bladder","C69"="Malignant neoplasm of eye and adnexa",
  "C70"="Malignant neoplasm of meninges","C71"="Malignant neoplasm of brain",
  "C73"="Malignant neoplasm of thyroid gland","C74"="Malignant neoplasm of adrenal gland",
  "C77"="Secondary malignant neoplasm of lymph nodes",
  "C78"="Secondary malignant neoplasm of respiratory organs",
  "C79"="Secondary malignant neoplasm of other sites",
  "C80"="Malignant neoplasm without specification of site","C81"="Hodgkin lymphoma",
  "C82"="Follicular lymphoma","C83"="Non-follicular lymphoma",
  "C85"="Non-Hodgkin lymphoma","C90"="Multiple myeloma","C91"="Lymphoid leukaemia",
  "C92"="Myeloid leukaemia","C95"="Leukaemia unspecified",
  "D00"="Carcinoma in situ of oral cavity",
  "D01"="Carcinoma in situ of other digestive organs",
  "D02"="Carcinoma in situ of respiratory system","D03"="Melanoma in situ",
  "D04"="Carcinoma in situ of skin","D05"="Carcinoma in situ of breast",
  "D06"="Carcinoma in situ of cervix uteri","D09"="Carcinoma in situ, other sites",
  "D10"="Benign neoplasm of mouth and pharynx","D12"="Benign neoplasm of colon and rectum",
  "D13"="Benign neoplasm of other digestive organs",
  "D14"="Benign neoplasm of respiratory system","D16"="Benign neoplasm of bone",
  "D17"="Benign lipomatous neoplasm","D18"="Haemangioma and lymphangioma",
  "D21"="Benign neoplasm of connective tissue","D22"="Melanocytic naevi",
  "D23"="Other benign neoplasms of skin","D24"="Benign neoplasm of breast",
  "D25"="Leiomyoma of uterus","D26"="Other benign neoplasms of uterus",
  "D27"="Benign neoplasm of ovary","D28"="Benign neoplasm of female genital organs",
  "D29"="Benign neoplasm of male genital organs","D30"="Benign neoplasm of urinary organs",
  "D31"="Benign neoplasm of eye","D33"="Benign neoplasm of brain",
  "D34"="Benign neoplasm of thyroid","D35"="Benign neoplasm of endocrine glands",
  "D36"="Benign neoplasm, other sites",
  "D37"="Neoplasm of uncertain behaviour, digestive organs",
  "D38"="Neoplasm of uncertain behaviour, respiratory organs",
  "D39"="Neoplasm of uncertain behaviour, female genital organs",
  "D40"="Neoplasm of uncertain behaviour, male genital organs",
  "D41"="Neoplasm of uncertain behaviour, urinary organs",
  "D44"="Neoplasm of uncertain behaviour, endocrine glands",
  "D45"="Polycythaemia vera","D46"="Myelodysplastic syndromes",
  "D47"="Neoplasms of uncertain behaviour, lymphoid tissue",
  "D48"="Neoplasm of uncertain behaviour, other sites",
  "D49"="Neoplasms of unspecified behaviour","D50"="Iron deficiency anaemia",
  "D51"="Vitamin B12 deficiency anaemia","D52"="Folate deficiency anaemia",
  "D53"="Other nutritional anaemias","D55"="Anaemia due to enzyme disorders",
  "D56"="Thalassaemia","D57"="Sickle-cell disorders",
  "D58"="Other hereditary haemolytic anaemias","D59"="Acquired haemolytic anaemia",
  "D60"="Acquired pure red cell aplasia","D61"="Other aplastic anaemias",
  "D62"="Acute posthaemorrhagic anaemia","D63"="Anaemia in chronic diseases",
  "D64"="Other anaemias","D65"="Disseminated intravascular coagulation",
  "D66"="Hereditary factor VIII deficiency","D67"="Hereditary factor IX deficiency",
  "D68"="Other coagulation defects","D69"="Purpura and other haemorrhagic conditions",
  "D70"="Agranulocytosis","D72"="Other disorders of white blood cells",
  "D73"="Diseases of spleen","D75"="Other diseases of blood",
  "D76"="Histiocytic and mast cell disorders",
  "D80"="Immunodeficiency with antibody defects","D81"="Combined immunodeficiencies",
  "D83"="Common variable immunodeficiency","D84"="Other immunodeficiencies",
  "D86"="Sarcoidosis","D89"="Other disorders involving immune mechanism",
  "E00"="Congenital iodine-deficiency syndrome",
  "E01"="Iodine-deficiency-related thyroid disorders","E03"="Other hypothyroidism",
  "E04"="Other non-toxic goitre","E05"="Thyrotoxicosis","E06"="Thyroiditis",
  "E07"="Other disorders of thyroid","E10"="Type 1 diabetes mellitus",
  "E11"="Type 2 diabetes mellitus","E13"="Other specified diabetes mellitus",
  "E14"="Unspecified diabetes mellitus","E16"="Other pancreatic endocrine disorders",
  "E20"="Hypoparathyroidism","E21"="Hyperparathyroidism",
  "E22"="Hyperfunction of pituitary gland","E23"="Hypofunction of pituitary gland",
  "E24"="Cushing syndrome","E25"="Adrenogenital disorders","E26"="Hyperaldosteronism",
  "E27"="Other disorders of adrenal gland","E28"="Ovarian dysfunction",
  "E29"="Testicular dysfunction","E34"="Other endocrine disorders","E40"="Kwashiorkor",
  "E43"="Severe protein-energy malnutrition",
  "E46"="Unspecified protein-energy malnutrition","E50"="Vitamin A deficiency",
  "E55"="Vitamin D deficiency","E56"="Other vitamin deficiencies",
  "E61"="Deficiency of other nutrient elements","E65"="Localised adiposity",
  "E66"="Obesity","E67"="Other hyperalimentation","E68"="Sequelae of hyperalimentation",
  "E70"="Amino-acid metabolism disorders",
  "E72"="Other disorders of amino-acid metabolism","E73"="Lactose intolerance",
  "E74"="Other disorders of carbohydrate metabolism",
  "E75"="Disorders of sphingolipid metabolism",
  "E76"="Disorders of glycosaminoglycan metabolism",
  "E78"="Disorders of lipoprotein metabolism",
  "E79"="Disorders of purine and pyrimidine metabolism",
  "E80"="Disorders of porphyrin metabolism","E83"="Disorders of mineral metabolism",
  "E84"="Cystic fibrosis","E85"="Amyloidosis","E86"="Volume depletion",
  "E87"="Other fluid-electrolyte disorders","E88"="Other metabolic disorders",
  "E89"="Postprocedural endocrine and metabolic disorders",
  "F00"="Dementia in Alzheimer disease","F01"="Vascular dementia",
  "F02"="Dementia in other diseases","F03"="Unspecified dementia","F05"="Delirium",
  "F06"="Other mental disorders due to brain damage",
  "F07"="Personality disorders due to brain disease",
  "F09"="Unspecified organic mental disorder",
  "F10"="Mental and behavioural disorders due to alcohol",
  "F11"="Mental and behavioural disorders due to opioids",
  "F12"="Mental and behavioural disorders due to cannabinoids",
  "F13"="Mental and behavioural disorders due to sedatives",
  "F14"="Mental and behavioural disorders due to cocaine",
  "F15"="Mental and behavioural disorders due to stimulants",
  "F16"="Mental and behavioural disorders due to hallucinogens",
  "F17"="Mental and behavioural disorders due to tobacco",
  "F19"="Mental and behavioural disorders due to multiple substances",
  "F20"="Schizophrenia","F21"="Schizotypal disorder",
  "F22"="Persistent delusional disorders","F23"="Acute psychotic disorders",
  "F25"="Schizoaffective disorders","F29"="Unspecified nonorganic psychosis",
  "F30"="Manic episode","F31"="Bipolar affective disorder","F32"="Depressive episode",
  "F33"="Recurrent depressive disorder","F34"="Persistent mood disorders",
  "F40"="Phobic anxiety disorders","F41"="Other anxiety disorders",
  "F42"="Obsessive-compulsive disorder","F43"="Reaction to severe stress",
  "F44"="Dissociative disorders","F45"="Somatoform disorders",
  "F48"="Other neurotic disorders","F50"="Eating disorders",
  "F51"="Nonorganic sleep disorders","F52"="Sexual dysfunction",
  "F55"="Abuse of non-dependence-producing substances",
  "F60"="Specific personality disorders","F61"="Mixed and other personality disorders",
  "F63"="Habit and impulse disorders","F70"="Mild intellectual disabilities",
  "F71"="Moderate intellectual disabilities","F72"="Severe intellectual disabilities",
  "F79"="Unspecified intellectual disabilities",
  "F80"="Specific developmental disorders of speech and language",
  "F81"="Specific developmental disorders of scholastic skills",
  "F84"="Pervasive developmental disorders","F90"="Hyperkinetic disorders",
  "F91"="Conduct disorders","F93"="Emotional disorders in childhood",
  "F94"="Disorders of social functioning in childhood","F95"="Tic disorders",
  "F98"="Other behavioural and emotional disorders in childhood",
  "F99"="Mental disorder without specification",
  "G00"="Bacterial meningitis","G03"="Meningitis due to other causes",
  "G04"="Encephalitis and myelitis","G10"="Huntington disease",
  "G11"="Hereditary ataxia","G12"="Spinal muscular atrophy","G20"="Parkinson disease",
  "G21"="Secondary parkinsonism","G24"="Dystonia",
  "G25"="Other extrapyramidal and movement disorders",
  "G30"="Alzheimer disease","G31"="Other degenerative diseases of nervous system",
  "G35"="Multiple sclerosis","G40"="Epilepsy","G41"="Status epilepticus",
  "G43"="Migraine","G44"="Other headache syndromes",
  "G45"="Transient cerebral ischaemic attacks","G47"="Sleep disorders",
  "G51"="Facial nerve disorders","G52"="Disorders of other cranial nerves",
  "G54"="Nerve root and plexus disorders","G56"="Mononeuropathies of upper limb",
  "G57"="Mononeuropathies of lower limb","G60"="Hereditary and idiopathic neuropathy",
  "G61"="Inflammatory polyneuropathy","G62"="Other polyneuropathies",
  "G70"="Myasthenia gravis","G71"="Primary disorders of muscles",
  "G80"="Cerebral palsy","G81"="Hemiplegia","G82"="Paraplegia and tetraplegia",
  "G83"="Other paralytic syndromes","G89"="Pain not elsewhere classified",
  "G90"="Disorders of autonomic nervous system","G91"="Hydrocephalus",
  "G93"="Other disorders of brain","G95"="Other diseases of spinal cord",
  "G96"="Other disorders of central nervous system",
  "G97"="Postprocedural disorders of nervous system",
  "G98"="Other disorders of nervous system",
  "H00"="Hordeolum and chalazion","H01"="Other inflammation of eyelid",
  "H02"="Other disorders of eyelid","H04"="Disorders of lacrimal system",
  "H05"="Disorders of orbit","H10"="Conjunctivitis",
  "H11"="Other disorders of conjunctiva","H15"="Disorders of sclera","H16"="Keratitis",
  "H17"="Corneal scars and opacities","H18"="Other disorders of cornea",
  "H20"="Iridocyclitis","H21"="Other disorders of iris and ciliary body",
  "H25"="Age-related cataract","H26"="Other cataract","H27"="Other disorders of lens",
  "H30"="Chorioretinal inflammation","H31"="Other disorders of choroid",
  "H33"="Retinal detachments","H34"="Retinal vascular occlusions",
  "H35"="Other retinal disorders","H40"="Glaucoma","H43"="Disorders of vitreous body",
  "H44"="Disorders of globe","H46"="Optic neuritis","H47"="Other disorders of optic nerve",
  "H50"="Other strabismus","H52"="Disorders of refraction and accommodation",
  "H53"="Visual disturbances","H54"="Blindness and low vision",
  "H57"="Other disorders of eye","H59"="Postprocedural disorders of eye",
  "H60"="Otitis externa","H61"="Other disorders of external ear",
  "H65"="Nonsuppurative otitis media","H66"="Suppurative and unspecified otitis media",
  "H68"="Eustachian tube disorders","H70"="Mastoiditis",
  "H71"="Cholesteatoma of middle ear","H72"="Perforation of tympanic membrane",
  "H73"="Other disorders of tympanic membrane",
  "H74"="Other disorders of middle ear and mastoid","H80"="Otosclerosis",
  "H81"="Disorders of vestibular function","H82"="Vertiginous syndromes",
  "H83"="Other diseases of inner ear","H90"="Conductive and sensorineural hearing loss",
  "H91"="Other hearing loss","H92"="Otalgia and effusion of ear",
  "H93"="Other disorders of ear","H95"="Postprocedural disorders of ear",
  "I00"="Rheumatic fever without heart involvement",
  "I05"="Rheumatic mitral valve diseases","I06"="Rheumatic aortic valve diseases",
  "I08"="Multiple valve diseases","I09"="Other rheumatic heart diseases",
  "I10"="Essential hypertension","I11"="Hypertensive heart disease",
  "I12"="Hypertensive renal disease","I13"="Hypertensive heart and renal disease",
  "I15"="Secondary hypertension","I20"="Angina pectoris",
  "I21"="Acute myocardial infarction","I22"="Subsequent myocardial infarction",
  "I25"="Chronic ischaemic heart disease","I26"="Pulmonary embolism",
  "I27"="Other pulmonary heart diseases","I30"="Acute pericarditis",
  "I33"="Acute and subacute endocarditis","I34"="Non-rheumatic mitral valve disorders",
  "I35"="Non-rheumatic aortic valve disorders","I42"="Cardiomyopathy",
  "I44"="Atrioventricular block","I45"="Other conduction disorders","I46"="Cardiac arrest",
  "I47"="Paroxysmal tachycardia","I48"="Atrial fibrillation and flutter",
  "I49"="Other cardiac arrhythmias","I50"="Heart failure","I51"="Other heart disease",
  "I60"="Subarachnoid haemorrhage","I61"="Intracerebral haemorrhage",
  "I63"="Cerebral infarction","I64"="Stroke, not specified",
  "I65"="Occlusion of precerebral arteries","I67"="Other cerebrovascular diseases",
  "I69"="Sequelae of cerebrovascular disease","I70"="Atherosclerosis",
  "I71"="Aortic aneurysm","I72"="Other aneurysm",
  "I73"="Other peripheral vascular diseases","I74"="Arterial embolism and thrombosis",
  "I77"="Other disorders of arteries","I80"="Phlebitis and thrombophlebitis",
  "I82"="Other venous embolism and thrombosis",
  "I83"="Varicose veins of lower extremities","I84"="Haemorrhoids",
  "I85"="Oesophageal varices","I87"="Other disorders of veins","I95"="Hypotension",
  "I97"="Postprocedural disorders of circulatory system",
  "J00"="Acute nasopharyngitis","J01"="Acute sinusitis","J02"="Acute pharyngitis",
  "J03"="Acute tonsillitis","J04"="Acute laryngitis and tracheitis",
  "J06"="Acute upper respiratory infections","J10"="Influenza",
  "J11"="Influenza due to unidentified influenza virus","J12"="Viral pneumonia",
  "J13"="Pneumonia due to Streptococcus pneumoniae","J15"="Bacterial pneumonia",
  "J18"="Pneumonia unspecified","J20"="Acute bronchitis","J21"="Acute bronchiolitis",
  "J30"="Vasomotor and allergic rhinitis","J31"="Chronic rhinitis and pharyngitis",
  "J32"="Chronic sinusitis","J33"="Nasal polyp","J34"="Other disorders of nose",
  "J35"="Chronic diseases of tonsils and adenoids",
  "J38"="Diseases of vocal cords and larynx",
  "J39"="Other diseases of upper respiratory tract","J40"="Bronchitis not specified",
  "J41"="Simple and mucopurulent chronic bronchitis",
  "J42"="Unspecified chronic bronchitis","J43"="Emphysema",
  "J44"="Other chronic obstructive pulmonary disease","J45"="Asthma",
  "J47"="Bronchiectasis","J60"="Coalworkers pneumoconiosis",
  "J67"="Hypersensitivity pneumonitis","J80"="Acute respiratory distress syndrome",
  "J81"="Pulmonary oedema","J84"="Other interstitial pulmonary diseases",
  "J86"="Pyothorax","J90"="Pleural effusion","J93"="Pneumothorax",
  "J94"="Other pleural conditions","J96"="Respiratory failure",
  "J98"="Other respiratory disorders",
  "K00"="Disorders of tooth development","K02"="Dental caries",
  "K04"="Diseases of pulp and periapical tissues",
  "K05"="Gingivitis and periodontal diseases","K07"="Dentofacial anomalies",
  "K08"="Other disorders of teeth","K11"="Diseases of salivary glands",
  "K12"="Stomatitis","K20"="Oesophagitis","K21"="Gastro-oesophageal reflux disease",
  "K22"="Other diseases of oesophagus","K25"="Gastric ulcer","K26"="Duodenal ulcer",
  "K29"="Gastritis and duodenitis","K30"="Functional dyspepsia",
  "K31"="Other diseases of stomach and duodenum","K35"="Acute appendicitis",
  "K37"="Unspecified appendicitis","K40"="Inguinal hernia","K41"="Femoral hernia",
  "K42"="Umbilical hernia","K43"="Ventral hernia","K44"="Diaphragmatic hernia",
  "K46"="Unspecified abdominal hernia","K50"="Crohn disease",
  "K51"="Ulcerative colitis","K52"="Other noninfective gastroenteritis and colitis",
  "K55"="Vascular disorders of intestine","K56"="Intestinal obstruction",
  "K57"="Diverticular disease","K58"="Irritable bowel syndrome",
  "K59"="Other functional intestinal disorders",
  "K60"="Fissure and fistula of anal region","K62"="Other diseases of anus and rectum",
  "K63"="Other diseases of intestine","K65"="Peritonitis","K70"="Alcoholic liver disease",
  "K71"="Toxic liver disease","K72"="Hepatic failure","K73"="Chronic hepatitis",
  "K74"="Fibrosis and cirrhosis of liver","K75"="Other inflammatory liver diseases",
  "K76"="Other diseases of liver","K80"="Cholelithiasis","K81"="Cholecystitis",
  "K82"="Other diseases of gallbladder","K83"="Other diseases of biliary tract",
  "K85"="Acute pancreatitis","K86"="Other diseases of pancreas",
  "K90"="Intestinal malabsorption","K91"="Postprocedural disorders of digestive system",
  "K92"="Other diseases of digestive system",
  "L00"="Staphylococcal scalded skin syndrome","L01"="Impetigo",
  "L02"="Cutaneous abscess","L03"="Cellulitis","L04"="Acute lymphadenitis",
  "L05"="Pilonidal cyst","L08"="Other local infections of skin",
  "L20"="Atopic dermatitis","L21"="Seborrhoeic dermatitis",
  "L23"="Allergic contact dermatitis","L24"="Irritant contact dermatitis",
  "L25"="Unspecified contact dermatitis","L28"="Lichen simplex chronicus and prurigo",
  "L29"="Pruritus","L30"="Other dermatitis","L40"="Psoriasis","L43"="Lichen planus",
  "L50"="Urticaria","L51"="Erythema multiforme","L52"="Erythema nodosum",
  "L53"="Other erythematous conditions","L55"="Sunburn","L60"="Nail disorders",
  "L63"="Alopecia areata","L64"="Androgenic alopecia","L65"="Other nonscarring hair loss",
  "L70"="Acne","L71"="Rosacea","L72"="Follicular cysts of skin",
  "L73"="Other follicular disorders","L80"="Vitiligo",
  "L81"="Other disorders of pigmentation","L82"="Seborrhoeic keratosis",
  "L84"="Corns and callosities","L85"="Other epidermal thickening",
  "L88"="Pyoderma gangrenosum","L89"="Pressure ulcer",
  "L90"="Atrophic disorders of skin","L91"="Hypertrophic disorders of skin",
  "L92"="Granulomatous disorders of skin","L93"="Lupus erythematosus",
  "L94"="Other localised connective tissue disorders",
  "L97"="Non-pressure chronic ulcer of lower limb","L98"="Other disorders of skin",
  "M00"="Pyogenic arthritis","M01"="Direct infections of joint",
  "M05"="Seropositive rheumatoid arthritis","M06"="Other rheumatoid arthritis",
  "M07"="Psoriatic and enteropathic arthropathies","M08"="Juvenile arthritis",
  "M10"="Gout","M11"="Other crystal arthropathies","M13"="Other arthritis",
  "M15"="Polyarthrosis","M16"="Coxarthrosis","M17"="Gonarthrosis",
  "M19"="Other arthrosis","M20"="Acquired deformities of fingers and toes",
  "M21"="Other acquired deformities of limbs","M23"="Internal derangement of knee",
  "M24"="Other specific joint derangements","M25"="Other joint disorders",
  "M30"="Polyarteritis nodosa","M32"="Systemic lupus erythematosus",
  "M33"="Dermatopolymyositis","M34"="Systemic sclerosis",
  "M35"="Other systemic involvement of connective tissue",
  "M40"="Kyphosis and lordosis","M41"="Scoliosis","M45"="Ankylosing spondylitis",
  "M46"="Other inflammatory spondylopathies","M47"="Spondylosis",
  "M48"="Other spondylopathies","M50"="Cervical disc disorders",
  "M51"="Other intervertebral disc disorders","M53"="Other dorsopathies",
  "M54"="Dorsalgia","M60"="Myositis","M62"="Other disorders of muscle",
  "M65"="Synovitis and tenosynovitis","M67"="Other disorders of synovium and tendon",
  "M70"="Soft tissue disorders related to use","M71"="Other bursopathies",
  "M72"="Fibroblastic disorders","M75"="Shoulder lesions",
  "M76"="Enthesopathies of lower limb","M77"="Other enthesopathies",
  "M79"="Other soft tissue disorders","M80"="Osteoporosis with pathological fracture",
  "M81"="Osteoporosis without pathological fracture","M83"="Adult osteomalacia",
  "M84"="Disorders of continuity of bone","M85"="Other disorders of bone density",
  "M86"="Osteomyelitis","M87"="Osteonecrosis","M89"="Other disorders of bone",
  "M94"="Other disorders of cartilage","M96"="Postprocedural musculoskeletal disorders",
  "M99"="Biomechanical lesions",
  "N00"="Acute nephritic syndrome","N02"="Recurrent haematuria",
  "N03"="Chronic nephritic syndrome","N04"="Nephrotic syndrome",
  "N10"="Acute tubulo-interstitial nephritis","N11"="Chronic tubulo-interstitial nephritis",
  "N13"="Obstructive uropathy","N17"="Acute kidney failure",
  "N18"="Chronic kidney disease","N19"="Unspecified kidney failure",
  "N20"="Calculus of kidney and ureter","N21"="Calculus of lower urinary tract",
  "N23"="Unspecified renal colic","N28"="Other disorders of kidney and ureter",
  "N30"="Cystitis","N31"="Neuromuscular dysfunction of bladder",
  "N32"="Other disorders of bladder","N34"="Urethritis and urethral syndrome",
  "N36"="Other disorders of urethra","N39"="Other disorders of urinary system",
  "N40"="Enlarged prostate","N41"="Inflammatory diseases of prostate",
  "N42"="Other disorders of prostate","N43"="Hydrocele and spermatocele",
  "N45"="Orchitis and epididymitis","N47"="Disorders of prepuce",
  "N48"="Other disorders of penis","N50"="Other disorders of male genital organs",
  "N60"="Benign mammary dysplasia","N61"="Inflammatory disorders of breast",
  "N63"="Unspecified lump in breast","N64"="Other disorders of breast",
  "N70"="Salpingitis and oophoritis","N71"="Inflammatory disease of uterus",
  "N72"="Inflammatory disease of cervix uteri",
  "N73"="Other female pelvic inflammatory diseases",
  "N76"="Other inflammation of vagina and vulva","N80"="Endometriosis",
  "N81"="Female genital prolapse","N83"="Non-inflammatory disorders of ovary",
  "N84"="Polyp of female genital tract","N85"="Other disorders of uterus",
  "N86"="Erosion and ectropion of cervix uteri","N87"="Dysplasia of cervix uteri",
  "N88"="Other disorders of cervix uteri","N89"="Other disorders of vagina",
  "N90"="Other disorders of vulva","N91"="Absent, scanty and rare menstruation",
  "N92"="Excessive and irregular menstruation","N93"="Other abnormal uterine bleeding",
  "N94"="Pain associated with female genital organs",
  "N95"="Menopausal and perimenopausal disorders","N97"="Female infertility",
  "N99"="Postprocedural disorders of genitourinary system",
  "O00"="Ectopic pregnancy","O03"="Spontaneous abortion",
  "O09"="Supervision of high-risk pregnancy",
  "O10"="Pre-existing hypertension complicating pregnancy",
  "O12"="Gestational oedema and proteinuria","O13"="Gestational hypertension",
  "O14"="Pre-eclampsia","O20"="Haemorrhage in early pregnancy",
  "O21"="Excessive vomiting in pregnancy","O22"="Venous complications in pregnancy",
  "O23"="Infections of genitourinary tract in pregnancy",
  "O24"="Diabetes mellitus in pregnancy",
  "O26"="Maternal care for conditions related to pregnancy","O30"="Multiple gestation",
  "O32"="Maternal care for malpresentation",
  "O34"="Maternal care for abnormality of pelvic organs",
  "O35"="Maternal care for foetal abnormality",
  "O36"="Maternal care for other foetal problems",
  "O41"="Other disorders of amniotic fluid","O42"="Premature rupture of membranes",
  "O43"="Placental disorders","O44"="Placenta praevia",
  "O45"="Premature separation of placenta","O48"="Late pregnancy","O60"="Preterm labour",
  "O62"="Abnormalities of forces of labour","O63"="Long labour",
  "O66"="Other obstructed labour","O68"="Labour complicated by foetal stress",
  "O70"="Perineal laceration during delivery","O72"="Postpartum haemorrhage",
  "O75"="Other complications of labour and delivery",
  "O80"="Full-term uncomplicated delivery","O82"="Caesarean delivery",
  "O85"="Puerperal sepsis","O86"="Other puerperal infections",
  "O87"="Venous complications in the puerperium","O90"="Complications of the puerperium",
  "O92"="Disorders of breast associated with childbirth",
  "O99"="Other maternal diseases classifiable elsewhere",
  "P05"="Disorders of newborn related to slow foetal growth",
  "P07"="Disorders related to short gestation","P20"="Intrauterine hypoxia",
  "P21"="Birth asphyxia","P22"="Respiratory distress of newborn",
  "P23"="Congenital pneumonia","P27"="Chronic respiratory disease of newborn",
  "P28"="Other respiratory conditions of newborn",
  "P29"="Cardiovascular disorders of newborn","P35"="Congenital viral diseases",
  "P36"="Bacterial sepsis of newborn",
  "P39"="Other infections specific to the perinatal period",
  "P52"="Intracranial haemorrhage of newborn","P55"="Haemolytic disease of newborn",
  "P59"="Neonatal jaundice from other causes",
  "P70"="Transitory disorders of carbohydrate metabolism",
  "P72"="Other transitory neonatal endocrine disorders",
  "P76"="Other intestinal obstruction of newborn",
  "P91"="Other disturbances of cerebral status of newborn",
  "P92"="Feeding problems of newborn","P94"="Disorders of muscle tone of newborn",
  "P96"="Other conditions of perinatal period","Q00"="Anencephaly","Q01"="Encephalocele",
  "Q02"="Microcephaly","Q03"="Congenital hydrocephalus","Q05"="Spina bifida",
  "Q10"="Congenital malformations of eyelid","Q12"="Congenital lens malformations",
  "Q16"="Congenital malformations of ear",
  "Q20"="Congenital malformations of cardiac chambers",
  "Q21"="Congenital malformations of cardiac septa",
  "Q24"="Other congenital malformations of heart",
  "Q25"="Congenital malformations of great arteries",
  "Q27"="Other congenital malformations of peripheral vascular system",
  "Q30"="Congenital malformations of nose","Q35"="Cleft palate","Q36"="Cleft lip",
  "Q37"="Cleft palate with cleft lip",
  "Q38"="Other congenital malformations of tongue and mouth",
  "Q39"="Congenital malformations of oesophagus",
  "Q40"="Other congenital malformations of upper alimentary tract",
  "Q43"="Other congenital malformations of intestine",
  "Q44"="Congenital malformations of gallbladder",
  "Q45"="Other congenital malformations of digestive system",
  "Q50"="Congenital malformations of ovaries",
  "Q53"="Undescended and ectopic testicle","Q54"="Hypospadias",
  "Q55"="Other congenital malformations of male genital organs","Q60"="Renal agenesis",
  "Q61"="Cystic kidney disease","Q63"="Other congenital malformations of kidney",
  "Q64"="Other congenital malformations of urinary system",
  "Q65"="Congenital deformities of hip","Q66"="Congenital deformities of feet",
  "Q69"="Polydactyly","Q70"="Syndactyly","Q76"="Congenital malformations of spine",
  "Q78"="Other osteochondrodysplasias",
  "Q79"="Congenital malformations of musculoskeletal system",
  "Q82"="Other congenital malformations of skin","Q85"="Phakomatoses",
  "Q87"="Other specified congenital malformation syndromes",
  "Q89"="Other congenital malformations","Q90"="Down syndrome",
  "Q91"="Trisomy 18 and trisomy 13","Q93"="Monosomies and deletions from autosomes",
  "Q96"="Turner syndrome","Q99"="Other chromosome abnormalities",
  "R00"="Abnormalities of heart beat","R04"="Haemorrhage from respiratory passages",
  "R05"="Cough","R06"="Abnormalities of breathing","R07"="Pain in throat and chest",
  "R09"="Other symptoms involving circulatory and respiratory systems",
  "R10"="Abdominal and pelvic pain","R11"="Nausea and vomiting","R12"="Heartburn",
  "R13"="Dysphagia","R14"="Flatulence and related conditions","R15"="Faecal incontinence",
  "R16"="Hepatomegaly and splenomegaly","R17"="Unspecified jaundice","R18"="Ascites",
  "R19"="Other symptoms involving digestive system",
  "R20"="Disturbances of skin sensation","R21"="Rash and other nonspecific skin eruption",
  "R22"="Localised swelling of skin","R25"="Abnormal involuntary movements",
  "R26"="Abnormalities of gait and mobility",
  "R29"="Other symptoms involving nervous and musculoskeletal systems",
  "R30"="Pain associated with micturition","R31"="Unspecified haematuria",
  "R32"="Unspecified urinary incontinence","R33"="Retention of urine","R35"="Polyuria",
  "R39"="Other symptoms involving urinary system",
  "R41"="Other symptoms involving cognitive functions",
  "R42"="Dizziness and giddiness","R43"="Disturbances of smell and taste",
  "R44"="Other symptoms involving general sensations",
  "R45"="Symptoms involving emotional state","R47"="Speech disturbances",
  "R50"="Fever","R51"="Headache","R52"="Pain","R53"="Malaise and fatigue",
  "R54"="Age-related physical debility","R55"="Syncope and collapse","R56"="Convulsions",
  "R57"="Shock","R59"="Enlarged lymph nodes","R60"="Oedema","R61"="Hyperhidrosis",
  "R62"="Lack of expected normal physiological development",
  "R63"="Symptoms and signs concerning food and fluid intake",
  "R65"="Signs specifically associated with systemic inflammation",
  "R68"="Other general symptoms and signs","R73"="Elevated blood glucose level",
  "R74"="Abnormal serum enzyme levels","R76"="Other abnormal immunological findings",
  "R79"="Other abnormal findings of blood chemistry","R80"="Proteinuria",
  "R82"="Other abnormal findings in urine",
  "R90"="Abnormal findings on diagnostic imaging of central nervous system",
  "R91"="Abnormal findings on diagnostic imaging of lung",
  "R93"="Abnormal findings on diagnostic imaging of other body structures",
  "R94"="Abnormal results of function studies",
  "R99"="Other ill-defined and unspecified causes of mortality",
  "S00"="Superficial injury of head","S02"="Fracture of skull and facial bones",
  "S06"="Intracranial injury","S12"="Fracture of cervical vertebra",
  "S22"="Fracture of rib, sternum and thoracic spine",
  "S32"="Fracture of lumbar spine and pelvis",
  "S36"="Injury of intra-abdominal organs","S42"="Fracture of shoulder and upper arm",
  "S52"="Fracture of forearm","S62"="Fracture at wrist and hand level",
  "S72"="Fracture of femur","S82"="Fracture of lower leg","S92"="Fracture of foot",
  "T00"="Superficial injuries involving multiple body regions",
  "T02"="Fractures involving multiple body regions","T07"="Unspecified multiple injuries",
  "T14"="Injury of unspecified body region","T36"="Poisoning by systemic antibiotics",
  "T38"="Poisoning by hormones","T39"="Poisoning by non-opioid analgesics",
  "T40"="Poisoning by narcotics","T42"="Poisoning by antiepileptic drugs",
  "T43"="Poisoning by psychotropic drugs","T45"="Poisoning by haematological agents",
  "T46"="Poisoning by cardiovascular agents","T50"="Poisoning by diuretics",
  "T51"="Toxic effect of alcohol","T56"="Toxic effect of metals",
  "T65"="Toxic effects of other substances",
  "T78"="Adverse effects not elsewhere classified",
  "T80"="Complications following infusion and transfusion",
  "T81"="Complications of procedures",
  "T82"="Complications of cardiac and vascular prosthetic devices",
  "T83"="Complications of genitourinary prosthetic devices",
  "T84"="Complications of internal orthopaedic prosthetic devices",
  "T85"="Complications of other internal prosthetic devices",
  "T86"="Failure and rejection of transplanted organs",
  "T88"="Other complications of surgical and medical care",
  "T90"="Sequelae of injuries of head","T91"="Sequelae of injuries of neck and trunk",
  "T92"="Sequelae of injuries of upper limb","T93"="Sequelae of injuries of lower limb"
)

## ── Load reference tables at startup ─────────────────────────────────────────
dis_colors_raw <- fread("Data/Disease_colors.txt", sep = "\t")
# Support both 3-col (disease, category, color) and 4-col (+ diseasename)
if (ncol(dis_colors_raw) >= 4) {
  setnames(dis_colors_raw, 1:4, c("disease","category","color","diseasename"))
  # Build name lookup from file, fill gaps with ICD10_NAMES then ""
  FILE_NAMES <- setNames(as.character(dis_colors_raw$diseasename), dis_colors_raw$disease)
} else {
  setnames(dis_colors_raw, 1:3, c("disease","category","color"))
  FILE_NAMES <- setNames(rep(NA_character_, nrow(dis_colors_raw)), dis_colors_raw$disease)
}
dis_colors <- dis_colors_raw[, .(disease, category, color)]
COLOR_VEC    <- setNames(dis_colors$color,    dis_colors$disease)
CATEGORY_VEC <- setNames(dis_colors$category, dis_colors$disease)

## Combined name lookup: file names > ICD10_NAMES > ""
icd_name <- function(code) {
  vapply(code, function(cd) {
    n <- FILE_NAMES[cd]
    if (!is.na(n) && nchar(n) > 0) return(n)
    n2 <- ICD10_NAMES[cd]
    if (!is.na(n2)) return(n2)
    ""
  }, character(1), USE.NAMES = FALSE)
}

## ── Sex-difference data ───────────────────────────────────────────────────────
prevalences <- fread("Data/prevalences.txt", sep = "\t")
# Compute total cases directly from case counts (total_cases column is not log10(women+men))
prevalences[, total_cases_raw := women + men]
# Disease name: prefer file column, then ICD10_NAMES lookup, then ""
prevalences[, disease_name := {
  fn <- as.character(diseasename)
  use_lookup <- is.na(fn) | fn == "NA" | fn == ""
  fn[use_lookup] <- icd_name(disease[use_lookup])
  fn
}]

age_diag <- fread("Data/age_diagnosis.txt", sep = "\t")
age_diag[, disease_name := {
  fn <- as.character(disease_name)
  use_lookup <- is.na(fn) | fn == "NA" | fn == ""
  fn[use_lookup] <- icd_name(Code[use_lookup])
  fn
}]

## ── Constants ─────────────────────────────────────────────────────────────────
ALL_WINDOWS <- c("0-1","0-2","0-3","0-4","0-5","1-2","2-3","3-4","4-5")
ALL_SEX     <- c("both","women","men")
SEX_LABELS  <- c(both="Both sexes", women="Women", men="Men")

## Helper: load one network file
load_network <- function(sex, window) {
  tok <- strsplit(window, "-")[[1]]
  fn  <- file.path("Data","networks",
                   paste0("RR_net_",sex,"_",tok[1],"_",tok[2],".txt"))
  if (!file.exists(fn)) return(NULL)
  dt <- fread(fn, sep = "\t")
  dt[, `:=`(
    sex       = sex,
    net_win   = window,
    name_a    = icd_name(disease_a),
    name_b    = icd_name(disease_b),
    color_a   = fcoalesce(COLOR_VEC[disease_a], "#AAAAAA"),
    color_b   = fcoalesce(COLOR_VEC[disease_b], "#AAAAAA"),
    category_a= fcoalesce(CATEGORY_VEC[disease_a], "Unknown"),
    category_b= fcoalesce(CATEGORY_VEC[disease_b], "Unknown"),
    dir_sig   = directionality_fdr < 0.05 & theta > 0.5
  )]
  dt
}

## Index disease list (from both/0-5; fallback to first available)
EXPLORER_DISEASES <- local({
  dt <- load_network("both","0-5")
  if (is.null(dt)) {
    for (s in ALL_SEX) for (w in ALL_WINDOWS) {
      dt <- load_network(s, w); if (!is.null(dt)) break
    }
  }
  if (is.null(dt)) return(character(0))
  codes <- sort(unique(c(dt$disease_a, dt$disease_b)))
  paste0(codes," – ",icd_name(codes))
})

## Category legend HTML
legend_html <- function() {
  cats <- unique(dis_colors[, .(category, color)])
  setorder(cats, category)
  items <- mapply(function(cat, col)
    sprintf('<div style="display:flex;align-items:center;margin-bottom:3px;">
      <div style="width:11px;height:11px;border-radius:50%%;background:%s;
                  margin-right:6px;flex-shrink:0;"></div>
      <span style="font-size:11px;">%s</span></div>', col, cat),
    cats$category, cats$color, SIMPLIFY = TRUE)
  HTML(paste(items, collapse=""))
}

## ============================================================
##  UI
## ============================================================
ui <- navbarPage(
  title = "Biological Clock of Multimorbidity",
  theme = shinytheme("flatly"),

  ## ── Tab 1 · Sex Differences ───────────────────────────────────────────────
  tabPanel("Sex Differences",
    sidebarLayout(
      sidebarPanel(width=3,
        h4("Plot type"),
        selectInput("sx_type","",
          choices=c(
            "Prevalence differences (volcano)" = "volcano",
            "Age at diagnosis differences"      = "age",
            "Age vs. prevalence (combined)"     = "combined"),
          selected="volcano"),
        hr(), h4("Category filter"),
        checkboxGroupInput("sx_cats","ICD-10 categories:",
          choices  = sort(unique(dis_colors$category)),
          selected = sort(unique(dis_colors$category))),
        hr(),
        ## volcano-specific
        conditionalPanel("input.sx_type=='volcano'",
          h5("Volcano options"),
          sliderInput("v_min_n","Min total cases (women + men):",0,500000,0,step=5000),
          checkboxInput("v_sig","Only sex-biased (p_adj<0.05 & |OR|>1.3)",FALSE)
        ),
        ## age-specific
        conditionalPanel("input.sx_type=='age'",
          h5("Age options"),
          sliderInput("a_min_n","Min total patients (women + men):",0,500000,0,step=5000),
          sliderInput("a_min_diff","Min |ΔAge| (years):",0,20,0,step=0.5),
          checkboxInput("a_sig","Only FDR<0.05",FALSE)
        ),
        ## combined-specific
        conditionalPanel("input.sx_type=='combined'",
          h5("Combined options"),
          sliderInput("c_min_or", "Min |log(OR)|:", 0,3,0,step=0.05),
          sliderInput("c_min_age","Min |ΔAge| (years):",0,20,0,step=0.5)
        )
      ),
      mainPanel(width=9,
        h3("Sex differences in disease prevalence and age at first diagnosis",
           align="center"),
        plotlyOutput("sx_plot", height="640px"), hr(),
        h4("Data table"), DTOutput("sx_table")
      )
    )
  ),

  ## ── Tab 2 · Comorbidity Networks ──────────────────────────────────────────
  tabPanel("Comorbidity Networks",
    sidebarLayout(
      sidebarPanel(width=3,
        h4("Network selection"),
        selectInput("net_win","Follow-up window:", ALL_WINDOWS, selected="0-5"),
        selectInput("net_sex","Population:",
          setNames(ALL_SEX, SEX_LABELS), selected="both"),
        hr(), h4("Filters"),
        checkboxInput("net_dir","Only directionally significant edges",FALSE),
        sliderInput("net_rr","Min RR:",1,30,1,step=0.5),
        sliderInput("net_n", "Min N patients (cases_event):",100,5000,100,step=50),
        hr(), h4("Node labels"),
        radioButtons("net_lbl","",
          choices=c("ICD-10 code"="code","Disease name"="name"),
          selected="code", inline=TRUE),
        hr(), h4("Category colours"), uiOutput("net_legend")
      ),
      mainPanel(width=9,
        h3("Temporal comorbidity network", align="center"),
        wellPanel(textOutput("net_info"),
                  style="padding:6px 12px;background:#f5f5f5;"),
        visNetworkOutput("net_vis", height="620px"), hr(),
        h4("Edge table"),
        p(em("Click a node to filter the table to its edges.")),
        DTOutput("net_tbl")
      )
    )
  ),

  ## ── Tab 3 · Disease Explorer ──────────────────────────────────────────────
  tabPanel("Disease Explorer",
    sidebarLayout(
      sidebarPanel(width=3,
        h4("Select a disease:"),
        selectInput("ex_dis","",
          choices  = EXPLORER_DISEASES,
          selected = if (length(EXPLORER_DISEASES)) EXPLORER_DISEASES[1] else NULL),
        hr(), h4("Population & window"),
        selectInput("ex_sex","Population:",
          setNames(ALL_SEX, SEX_LABELS), selected="both"),
        selectInput("ex_win","Window:", ALL_WINDOWS, selected="0-5"),
        hr(), h4("Filters"),
        checkboxInput("ex_dir","Only directionally significant edges",FALSE),
        sliderInput("ex_rr","Min RR:",1,30,1,step=0.5),
        sliderInput("ex_n", "Min N patients:",100,5000,100,step=50),
        hr(), h4("Table options"),
        checkboxInput("ex_allwin","Compare RR across all windows",FALSE),
        hr(), h4("Category colours"), uiOutput("ex_legend")
      ),
      mainPanel(width=9,
        h3("Comorbidities of the selected disease", align="center"),
        visNetworkOutput("ex_vis", height="560px"), hr(),
        h4("Comorbidity table"), DTOutput("ex_tbl")
      )
    )
  ),

  ## ── Tab 4 · Documentation ─────────────────────────────────────────────────
  tabPanel("Documentation",
    fluidPage(fluidRow(column(width=10, offset=1,
      h2("About this application", align="center"), br(),
      h4("Data source"),
      p("Analysis of 11 years of EHR data from 5,821,197 individuals in Catalan
        primary care (SIDIAP, 2008–2018), across nine follow-up windows."),
      h4("Comorbidity networks"),
      p("Nodes = ICD-10 three-digit diseases (coloured by chapter). Directed edges
        represent significant comorbidity associations (LFSR ≤ 0.05,
        CI_low_RR ≥ 1.01, N ≥ 100). Edge width scales with RR (capped at 20).
        Orange edges are directionally significant (dir. FDR < 0.05, θ > 0.5);
        grey edges are not."),
      h4("Follow-up windows"),
      tags$ul(
        tags$li(strong("Cumulative 0–k years:"),
          " total probability of developing disease B by year k."),
        tags$li(strong("Conditional k–(k+1) years:"),
          " risk in a specific annual interval given disease B was not diagnosed
            before. Captures late-emerging or residual risk.")
      ),
      h4("Sex differences tab"),
      tags$ul(
        tags$li(strong("Volcano:")," log(OR) of prevalence in women vs. men
          against total cases."),
        tags$li(strong("Age at diagnosis:")," mean age difference (women − men)
          at first diagnosis."),
        tags$li(strong("Combined:")," both dimensions simultaneously.")
      ),
      h4("Disease Explorer"),
      p("Select an index disease to view its ego-network.
        Enable 'Compare RR across all windows' for a wide-format table showing
        how comorbidity risk evolves over time."),
      h4("Reference"),
      p(em("Sánchez-Valle J et al. The biological clock of multimorbidity:
           temporal dynamics of disease co-occurrence in primary care.
           Preprint, 2025."))
    )))
  )
)

## ============================================================
##  SERVER
## ============================================================
server <- function(input, output, session) {

  ## legends
  output$net_legend <- renderUI(legend_html())
  output$ex_legend  <- renderUI(legend_html())

  ## ===========================================================================
  ##  TAB 1 · Sex Differences
  ## ===========================================================================

  vol_dt <- reactive({
    dt <- prevalences[as.character(diseasecategory) %in% input$sx_cats]
    dt <- dt[total_cases_raw >= input$v_min_n]
    if (input$v_sig)
      dt <- dt[p_adj < 0.05 & (exp(OR) > 1.3 | exp(OR) < 1/1.3)]
    dt
  })

  age_dt <- reactive({
    dt <- age_diag[catename %in% input$sx_cats]
    dt[, total_cases := women + men]   # case counts per disease
    dt <- dt[total_cases >= input$a_min_n]
    dt <- dt[abs(DiffMean) >= input$a_min_diff]
    if (input$a_sig) dt <- dt[FDR < 0.05]
    dt
  })

  comb_dt <- reactive({
    dp <- prevalences[as.character(diseasecategory) %in% input$sx_cats,
                      .(disease, diseasecategory, color, OR, total_cases_raw, disease_name)]
    da <- age_diag[catename %in% input$sx_cats, .(Code, DiffMean, FDR)]
    dt <- merge(dp, da, by.x="disease", by.y="Code")
    dt[abs(OR) >= input$c_min_or & abs(DiffMean) >= input$c_min_age]
  })

  output$sx_plot <- renderPlotly({
    ptype <- input$sx_type

    if (ptype == "volcano") {
      dt <- vol_dt(); if (!nrow(dt)) return(plotly_empty())
      p <- ggplot(dt,
            aes(x=log10(total_cases_raw), y=OR, color=color,
                text=paste0("Code: ",disease,"<br>Name: ",disease_name,
                            "<br>Category: ",diseasecategory,
                            "<br>log(OR): ",round(OR,3),
                            "<br>OR: ",round(exp(OR),2),
                            "<br>N (women+men): ",total_cases_raw))) +
        geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
        geom_point(size=2.2, alpha=0.75) +
        scale_color_identity() +
        labs(x="log10(Total cases: women + men)", y="log(OR)  women vs. men") +
        theme_bw(base_size=13) + theme(panel.grid.minor=element_blank())

    } else if (ptype == "age") {
      dt <- age_dt(); if (!nrow(dt)) return(plotly_empty())
      p <- ggplot(dt,
            aes(x=log10(total_cases), y=DiffMean, color=color,
                text=paste0("Code: ",Code,"<br>Name: ",disease_name,
                            "<br>Category: ",catename,
                            "<br>ΔAge (W−M): ",round(DiffMean,2)," yrs",
                            "<br>Mean age women: ",round(WomenMean,1)," yrs",
                            "<br>Mean age men: ",round(MenMean,1)," yrs",
                            "<br>N (women+men): ",total_cases,
                            "<br>FDR: ",signif(FDR,3)))) +
        geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
        geom_point(size=2.2, alpha=0.75) +
        scale_color_identity() +
        labs(x="log10(Total cases: women + men)",
             y="Mean age difference  (Women − Men, years)") +
        theme_bw(base_size=13) + theme(panel.grid.minor=element_blank())

    } else {
      dt <- comb_dt(); if (!nrow(dt)) return(plotly_empty())
      p <- ggplot(dt,
            aes(x=DiffMean, y=OR, color=color,
                text=paste0("Code: ",disease,"<br>Name: ",disease_name,
                            "<br>Category: ",diseasecategory,
                            "<br>ΔAge (W−M): ",round(DiffMean,2)," yrs",
                            "<br>log(OR): ",round(OR,3),
                            "<br>OR: ",round(exp(OR),2)))) +
        geom_vline(xintercept=0, linetype="dashed", colour="grey70") +
        geom_hline(yintercept=0, linetype="dashed", colour="grey70") +
        geom_point(size=2.8, alpha=0.75) +
        scale_color_identity() +
        labs(x="Mean age difference  (Women − Men, years)",
             y="log(OR)  women vs. men  (prevalence)") +
        theme_bw(base_size=13) + theme(panel.grid.minor=element_blank())
    }

    ggplotly(p, tooltip="text")
  })

  output$sx_table <- renderDT({
    ptype <- input$sx_type
    out <- if (ptype=="volcano") {
      vol_dt()[, .(Code=disease, Name=disease_name,
                   Category=as.character(diseasecategory),
                   `log(OR)`=round(OR,3), OR=round(exp(OR),2),
                   `N (women+men)`=total_cases_raw, Women=women, Men=men,
                   `p adj`=signif(p_adj,3), `Sex bias`=sex_bias)]
    } else if (ptype=="age") {
      age_dt()[, .(Code, Name=disease_name, Category=catename,
                   `DeltaAge (W-M)`=round(DiffMean,2),
                   `Mean age W`=round(WomenMean,1),
                   `Mean age M`=round(MenMean,1),
                   `N (women+men)`=total_cases,
                   FDR=signif(FDR,3))]
    } else {
      comb_dt()[, .(Code=disease, Name=disease_name,
                    Category=as.character(diseasecategory),
                    `DeltaAge (W-M)`=round(DiffMean,2),
                    `log(OR)`=round(OR,3), OR=round(exp(OR),2),
                    `N (women+men)`=total_cases_raw)]
    }
    datatable(out,
      extensions="Buttons",
      options=list(dom="Bfrtip", buttons=c("csv","excel"),
                   pageLength=15, scrollX=TRUE),
      rownames=FALSE)
  })

  ## ===========================================================================
  ##  TAB 2 · Comorbidity Networks
  ## ===========================================================================

  net_raw <- reactive({ load_network(input$net_sex, input$net_win) })

  net_flt <- reactive({
    dt <- net_raw(); if (is.null(dt)||!nrow(dt)) return(NULL)
    dt <- dt[RR_shrunk >= input$net_rr & cases_event >= input$net_n]
    if (input$net_dir) dt <- dt[dir_sig==TRUE]
    dt
  })

  output$net_info <- renderText({
    dt <- net_flt()
    if (is.null(dt)||!nrow(dt)) return("No edges match current filters.")
    nn <- length(unique(c(dt$disease_a, dt$disease_b)))
    paste0("Window: ", input$net_win,
           "   |   Population: ", SEX_LABELS[input$net_sex],
           "   |   Nodes: ", nn,
           "   |   Edges: ", nrow(dt))
  })

  output$net_vis <- renderVisNetwork({
    dt <- net_flt(); if (is.null(dt)||!nrow(dt)) return(NULL)
    use_name <- input$net_lbl == "name"

    nodes <- unique(rbindlist(list(
      dt[, .(id=disease_a,
             label=if(use_name) name_a else disease_a,
             title=paste0("<b>",disease_a,"</b> – ",name_a,"<br>",category_a),
             color=color_a)],
      dt[, .(id=disease_b,
             label=if(use_name) name_b else disease_b,
             title=paste0("<b>",disease_b,"</b> – ",name_b,"<br>",category_b),
             color=color_b)]
    )), by="id")

    edges <- dt[, .(
      from  = disease_a, to = disease_b,
      value = pmin(RR_shrunk, 20),
      title = paste0("RR: ",round(RR_shrunk,2),
                     " [",round(CI_low_RR_shrunk,2),
                     "–",round(CI_high_RR_shrunk,2),"]",
                     "<br>N: ",cases_event,
                     "<br>θ: ",round(theta,3),
                     "<br>Dir.sig: ",dir_sig),
      color = ifelse(dir_sig,"#E06C00","#AAAAAA"),
      dashes = !dir_sig
    )]

    visNetwork(nodes, edges) %>%
      visExport() %>%
      visEdges(arrows="to",
               smooth=list(enabled=TRUE,type="curvedCW",roundness=0.2)) %>%
      visOptions(
        highlightNearest=list(enabled=TRUE,degree=1,
                              algorithm="hierarchical",labelOnly=FALSE),
        nodesIdSelection=list(enabled=TRUE,
                              style="width:300px;height:26px",
                              main="Select a disease")
      ) %>%
      visIgraphLayout(layout="layout_with_fr", randomSeed=42) %>%
      visInteraction(multiselect=TRUE) %>%
      visEvents(select="function(nodes){
                  Shiny.onInputChange('net_node_sel', nodes.nodes);
                }")
  })

  output$net_tbl <- renderDT({
    dt  <- net_flt(); if (is.null(dt)||!nrow(dt)) return(datatable(data.frame()))
    sel <- input$net_node_sel
    if (!is.null(sel) && length(sel) && nchar(sel[1]))
      dt <- dt[disease_a %in% sel | disease_b %in% sel]
    out <- dt[, .(
      `Index (A)`=disease_a, `Index name`=name_a, `Cat. A`=category_a,
      `Comorbidity (B)`=disease_b, `Comorbidity name`=name_b, `Cat. B`=category_b,
      RR=round(RR_shrunk,2),
      `CI low`=round(CI_low_RR_shrunk,2), `CI high`=round(CI_high_RR_shrunk,2),
      `N pts`=cases_event, `theta`=round(theta,3), `Dir.sig`=dir_sig,
      Window=net_win, Sex=sex
    )]
    datatable(out,
      extensions="Buttons",
      options=list(dom="Bfrtip",buttons=c("csv","excel"),
                   pageLength=15,scrollX=TRUE),
      rownames=FALSE)
  })

  ## ===========================================================================
  ##  TAB 3 · Disease Explorer
  ## ===========================================================================

  ex_code <- reactive({ sub(" –.*","", input$ex_dis) })

  ex_single <- reactive({
    dt <- load_network(input$ex_sex, input$ex_win); if (is.null(dt)) return(NULL)
    code <- ex_code()
    dt <- dt[(disease_a==code | disease_b==code) &
               RR_shrunk >= input$ex_rr & cases_event >= input$ex_n]
    if (input$ex_dir) dt <- dt[dir_sig==TRUE]
    dt
  })

  ex_allwin <- reactive({
    code <- ex_code()
    rbindlist(lapply(ALL_WINDOWS, function(w) {
      dt <- load_network(input$ex_sex, w); if (is.null(dt)) return(NULL)
      dt <- dt[(disease_a==code|disease_b==code) &
                 RR_shrunk>=input$ex_rr & cases_event>=input$ex_n]
      if (input$ex_dir) dt <- dt[dir_sig==TRUE]
      dt
    }), fill=TRUE)
  })

  output$ex_vis <- renderVisNetwork({
    code <- ex_code()
    dt   <- ex_single(); if (is.null(dt)||!nrow(dt)) return(NULL)

    ## neighbours
    nb <- unique(rbindlist(list(
      dt[disease_b==code, .(id=disease_a, label=disease_a,
          title=paste0("<b>",disease_a,"</b> – ",name_a,"<br>",category_a),
          color=color_a)],
      dt[disease_a==code, .(id=disease_b, label=disease_b,
          title=paste0("<b>",disease_b,"</b> – ",name_b,"<br>",category_b),
          color=color_b)]
    )), by="id")[id != code]

    idx  <- data.table(id=code, label=code,
                       title=paste0("<b>",code,"</b> – ",icd_name(code)," (index)"),
                       color="#222222", size=28)
    nb[, size := 18]
    nodes <- rbindlist(list(idx, nb), fill=TRUE)

    edges <- dt[, .(
      from=disease_a, to=disease_b,
      value=pmin(RR_shrunk,20),
      title=paste0("RR: ",round(RR_shrunk,2),
                   " [",round(CI_low_RR_shrunk,2),
                   "–",round(CI_high_RR_shrunk,2),"]",
                   "<br>N: ",cases_event,
                   "<br>θ: ",round(theta,3),
                   "<br>Dir.sig: ",dir_sig),
      color=ifelse(dir_sig,"#E06C00","#AAAAAA"),
      dashes=!dir_sig
    )]

    visNetwork(nodes, edges) %>%
      visExport() %>%
      visEdges(arrows="to",
               smooth=list(enabled=TRUE,type="curvedCW",roundness=0.2)) %>%
      visOptions(highlightNearest=list(enabled=TRUE,degree=1,
                                       algorithm="hierarchical",labelOnly=FALSE)) %>%
      visIgraphLayout(layout="layout_with_fr", randomSeed=42)
  })

  output$ex_tbl <- renderDT({
    code <- ex_code()

    if (input$ex_allwin) {
      dt <- ex_allwin()
      if (is.null(dt)||!nrow(dt)) return(datatable(data.frame()))
      ## normalise direction: index always as disease_a
      dt2 <- copy(dt)
      flip <- dt2$disease_b == code
      dt2[flip, `:=`(
        disease_a=disease_b, name_a=name_b, category_a=category_b,
        disease_b=disease_a, name_b=name_a, category_b=category_a
      )]
      wide <- dcast(dt2,
        disease_a+name_a+disease_b+name_b+category_b ~ net_win,
        value.var="RR_shrunk", fun.aggregate=mean)
      wcols <- intersect(ALL_WINDOWS, names(wide))
      wide[, (wcols) := lapply(.SD, round, 2), .SDcols=wcols]
      setnames(wide,
        c("disease_a","name_a","disease_b","name_b","category_b"),
        c("Index","Index name","Comorbidity","Comorbidity name","Category"))
      out <- wide

    } else {
      dt <- ex_single()
      if (is.null(dt)||!nrow(dt)) return(datatable(data.frame()))
      out <- dt[, .(
        `Index (A)`=disease_a, `Index name`=name_a, `Cat. A`=category_a,
        `Comorbidity (B)`=disease_b, `Comorbidity name`=name_b, `Cat. B`=category_b,
        RR=round(RR_shrunk,2),
        `CI low`=round(CI_low_RR_shrunk,2), `CI high`=round(CI_high_RR_shrunk,2),
        `N pts`=cases_event, `theta`=round(theta,3), `Dir.sig`=dir_sig,
        Window=net_win, Sex=sex
      )]
    }

    datatable(out,
      extensions="Buttons",
      options=list(dom="Bfrtip",buttons=c("csv","excel"),
                   pageLength=20,scrollX=TRUE),
      rownames=FALSE)
  })

}  # end server

shinyApp(ui=ui, server=server)
