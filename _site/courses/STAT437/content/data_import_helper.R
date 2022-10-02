################################################################################
# Treatment of Lead-Exposed Children
#   https://pubmed.ncbi.nlm.nih.gov/9690266/
# 
# Data Columns:
#   ID: Subject ID Number
#   Treatment: Which treatment group (P=Placebo; S=Succimenr)
#   W0, W1, W4, W6: Blood-lead levels in micrograms per deciliter at Weeks 0, 1, 4, and 6
TLC <- read.csv("data/TLC/TLC.csv")

################################################################################
# OASIS
#   https://www.kaggle.com/jboysen/mri-and-alzheimers
#   https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2895005/
# 
# Data Columns:
#   Subject.ID: Subject ID Number
#   MRI.ID: MRI Scan Number
#   Group: Dementia Status
#   Visit: Which visit Number
#   MR.Delay: MRI Delay time for Contrast (Seconds)
#   M.F: Assigned sex (M = Male; F = Female)
#   Hand: Which is their dominant hand? (R = Right, L = Left)
#   Age: How old at visit (Years)
#   EDUC: Number of years of education
#   SES: Socio-economic status -- Hollingshead Index of Social Position and classified into categories from 1 (highest status) to 5 (lowest status) (Hollingshead, 1957)
#   MMSE: Mini-Mental State Examination score (range is from 0 [worst] to 30 [best]) (Folstein, Folstein, & McHugh, 1975)
#   CDR: Clinical Dementia Rating. (0 = no dementia, 0.5 = very mild AD, 1 = mild AD, 2. = moderate AD) (Morris, 1993)
#   ASF: Atlas scaling factor (unitless). Computed scaling factor that transforms native-space brain and skull to the atlas target (i.e. the determinant of the transform matrix) (Buckner et al., 2004)
#   eTIV: Estimated total intracranial volume (cm3) (Buckner et al., 2004)
#   nWBV: Normalized whole brain volume, expressed as a percent of all voxels in the atlas-masked image that are labeled as gray or white matter by the automated tissue segmentation process (Fotenos et al., 2005)
OASIS <- read.csv("data/OASIS/oasis_longitudinal.csv")

################################################################################
#  Korea Welfare Panel Study (KOWEPS).
#   https://www.kaggle.com/hongsean/korea-income-and-welfare
#   https://www.koweps.re.kr:442/eng/main.do
#
# Data Columns:
#   ID: Subject ID 
#   year: Year of the observation
#   wave: Which study wave [1 (2005) through 14 (2018)]
#   Region: 1) Seoul 2) Kyeong-gi 3) Kyoung-nam 4) Kyoung-buk 5) Chung-nam 6) Gang-won &. Chung-buk 7) Jeolla & Jeju
#   Income: Measured in Million KRW 
#   family_member: Number of Family Members
#   gender: Assigned sex (1) = Male; (2) = Female
#   year_born: Birth Year
#   education_level: (1) no education(under 7 yrs-old) 
#                    (2) no education(7 & over 7 yrs-old) 
#                    (3) elementary 
#                    (4) middle school 
#                    (5) high school 
#                    (6) college 
#                    (7) university degree (8) MA (9) doctoral degree
#   marriage: marital status. 1) not applicable (under 18) 2) married 3) separated by death 4) separated 5) not married yet 6) others
#   religion: 1) religious 2) not religious
#   occuptation: Job ID code [reference provided in separate file]
#   reason_none_worker: (1) not able 
#                       (2) in military service 
#                       (3) studying in school 
#                       (4) preparing for school 
#                       (5) preparing to apply job 
#                       (6) house worker 
#                       (7) caring for kids at home 
#                       (8) nursing 
#                       (9) giving-up on economic activities 
#                       (10) no intention to work 
#                       (11) others
KOWEPS <- read.csv("data/Korea Income and Welfare/Korea Income and Welfare.csv")
job_reference <- read.csv("data/Korea Income and Welfare/job_code_table.csv")

################################################################################
# Pasta Sales Data
#   https://archive-beta.ics.uci.edu/ml/datasets/hierarchical+sales+data
#   https://www.sciencedirect.com/science/article/abs/pii/S0957417421005431?via%3Dihub
#
# Data Columns:
#     DATE: Day of sales record
#     brand: Number representing which brand {1,2,3,4}
#     prod: Number representing which product (within the brand) 1--45
#     QTY: Quantity of that product sold on the day
#     PROMO: Indication {1, 0} of whether or not the product was on sale
pasta <- read.csv("data/Sales Data/pasta_sales.csv")

################################################################################
# Podcast Data
#   https://ssc.ca/en/case-study/predicting-podcast-popularity-itunes
#   Note: the sentiment scores were added by Melissa Van Bussel (https://www.melissavanbussel.com/)
#
# Data Columns:
#       id: Podcast Identifier
#       rating_value: Star rating on the Date (out of 5)
#       number_of_reviews: Numeric values for the total number of podcast reviews
#       date: The date that the information was recorded
#       release: The date/time of the last podcast release
#       title: A character string of the podcast name
#       subcategory: The category of the podcast
#       pod_length: The length of the most recent podcast epsiode.
#       avg_*: Sentiment scores for the podcast description, based on "Facebook" reaction statuses
#              The method of scoring uses a training set of Facebook statuses with user reactions to 
#               try to assess the emotional content of the post.
#       avg_*_rvw: The same sentiment scores as above, applied to an analysis of the text reviews
#                   left on these podcasts.
podcast <- read.csv("data/Podcast/podcast_data.csv",
                    colClasses=c("date"="Date", "release"="character", "title"="character", "id"="character"))
podcast$release <- as.POSIXlt(podcast$release, tz="EST")

################################################################################
# Dental Data
#   Data made available through Dr. Leilei Zeng (University of Waterloo)
#   
# Data Columns:
#     ID: the patient ID
#     Sex: a character "F" for female, "M" for male
#     Y1: Dental measurement at the age of 8 (mm)
#     Y2: Dental measurement at the age of 10 (mm)
#     Y3: Dental measurement at the age of 12 (mm)
#     Y4: Dental measurement at the age of 14 (mm)
dental <- read.csv("data/Dental/dental.csv")

################################################################################
# Dog Data
#   Bouma, Esther M.C.; Vink, Lonneke M.; Dijkstra, Arie, 2020, "Replication Data for: Expectations versus reality: long-term research on the dog–owner relationship", https://doi.org/10.34894/ZCKPKO, DataverseNL, V1
#   https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/ZCKPKO
#   https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7278369/
#   
# Data Columns:
#    ID: Numeric ID for the Subject
#    PreviousExperience: [1 = Yes, 0 = No]
#    DogHistory: [1 = Yes (Before T0), 2 = Yes (Current), 3 = No]
#    Sex: [1 = Female, 2 = Male]
#    Age: Continuous, at T0 
#    SingleAdult: [1 = Yes, 0 = No (Couple or Multiple Adults)]
#    Kids: [1 = Yes, 0 = No]
#    DogSex: [1 = Female, 2 = Male]
#    DogProfessional: [1 = Works with Dogs, 0 = Does Not Work with Dogs]
#    DogTrainingCourse: [1 = Has ever taken dog training course, 0 = Has not ever taken dog training course]
#    Time: 0 = Before Receiving Dog; 1 = 6 Months after Dog; 2 = 18 Months after Dog
# Outcomes: All outcomes are based on likert scale survey responses.
#    SocialComparison
#    Commitment
#    Satisfaction
dogs <- read.csv("data/Dog/DogOwners.csv")

################################################################################
# Ontario COVID-19 Data
#   Downloaded from https://covid-19.ontario.ca/data/testing-volumes-and-results
#   Transformed/Augmented by Dylan Spicker
#
# Data Columns:
#   ID: The ID for the local public health unit (individual)
#   date: The date that the testing data was completed on
#   time_idx: A scaled time index (starting at t=0) instead of the date
#   estimated_pop: The estimated population for the public health unit (based on provided data)
#   positivity_rate: The proportion of tests that were returned positive
#   test_volume: the number of completed tests
covid <- read.csv("data/COVID/ontario_by_phu.csv")

################################################################################
# Epilepsy Seizure Count Data
#   https://content.sph.harvard.edu/fitzmaur/ala2e/epilepsy.txt
#   Applied LDA: Garrett Fitzmaurice, Nan Laird & James Ware
#
# Data Columns
#   ID: Patient ID 
#   Trt: Treatment (0=Placebo, 1=Progabide)
#   Age: Patient age
#   y{j}: Seizure count for interval 'j'
#   offset{j}: The length of time (in weeks) of interval 'j'
seizures <- read.csv("data/Epilepsy/seizures_full.csv")

################################################################################
# Six Cities Air Pollution Data
#   https://content.sph.harvard.edu/fitzmaur/ala2e/
#   Applied LDA: Garrett Fitzmaurice, Nan Laird & James Ware
#   Dockery, D.W., Berkey, C.S., Ware, J.H., Speizer, F.E. and Ferris, B.G. (1983). Distribution of FVC and FEV1 in children 6 to 11 years old. American Review of Respiratory Disease, 128, 405-412.
#
# Data Columns
#   id: Patient ID 
#   ht: Patient height at the corresponding visit
#   age: Patient age
#   baseht: Patient height at the first visit
#   baseage: Patient age at the first visit
#   logfev1: The log of FEV1 measurement (outcome based on lung function)
air_pollution <- read.csv("data/Six Cities/air_pollution.csv")

################################################################################
# Financial Crisis: A Longitudinal Study of Public Response
#   https://www.icpsr.umich.edu/web/ICPSR/studies/36341
#   Burns, William. Financial Crisis: A Longitudinal Study of Public Response. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2016-01-25. https://doi.org/10.3886/ICPSR36341.v1
# 
# Data Columns
#   id: Individual ID
#   age: Individual age at baseline
#   political_affiliation: Political party affilitation [5 Categories; Democrat, Republican, Independent, Undeclared, Other]
#   libcon: Individual's score on a Liberal-Conservative scale [4 Categories; Very Conservative, Conservative, Liberal, Very Liberal]
#   ownhouse: Whether an individual owns a home at baseline
#   sex: The sex of the individual
#   income: The income level of the individual, at baseline [6 Categories]
#   highested: The levels of highest attained education for the individual, at baseline [7 Categories]
#   employment: The individual's employment status at baseline [5 Categories; No, Yes full-time, Yes part-time, Retired, Yes (at home)]
#   Yj: The outcome at time j (=2,3,...,8). Categorical variable based on the sentiment regarding Wallstreet at time j. [5 Categories; Very Negative to Very Positive]
wallstreet <- read.csv("data/Wallstreet/wallstreet_sentiment.csv")

################################################################################
# Schoolgirls Data
#   Data made available through Dr. Leilei Zeng (University of Waterloo)
# 
# Data Columns
#   id: Individual ID
#   age: Individual's age at the corresponding visit
#   momheight: Categorical variable for mother's height at girl's birth (1=Short; 2=Medium; 3=Tall)
#   height: Girl's height at the corresponding visit.
schoolgirls <- read.csv("data/School Girls/schoolgirls.csv")

################################################################################
# Customer Churn Data
#   https://community.ibm.com/community/user/businessanalytics/blogs/steven-macko/2019/07/11/telco-customer-churn-1113
#
# Data Columns
#   id: Individual ID
#   time: measurement of event time, either censoring or event, in months
#   censored: whether it was censored (1) or not (0)
#   total_charges: the amount that the customer has paid, over the whole tenure with the company
#   monthly_charges: the current amount that the customer pays per month
#   contract: the type of contract, one of (i) "Month-to-month", (ii) "One year", or (iii) "Two year"
#   payment_method: How the customer pays, one of (i) "Bank transfer (automatic)", (ii) "Credit card (automatic)",
#                   (iii) "Electronic check", or (iv) "Mailed check"
#   paperless: "Yes" or "No" factor variable of whether the billing is paperless
#   gender: The identified sex of the individual either "Female" or "Male"      
#   senior: "Yes" or "No" factor variable of whether the individual is a senior
#   partner: "Yes" or "No" factor variable of whether the individual has a partner
#   dependents: "Yes" or "No" factor variable of whether the individual has dependents
#   phone_service: "Yes" or "No" factor variable of whether the individual pays for phone service
#   multiple_lines: A factor variable indicating whether the individual has multiple phone lines or not
#                   Levels for "Yes", "No", or "No phone service"
#   internet_service: A factor variable indicatign the type of phone service the individual has
#                     Levels for "DSL", "Fibre optic", or "No"
#   online_security: A factor variable with 3 levels ("Yes", "No", or "No internet service") for whether the individual pays for online security services
#   online_backup: A factor variable with 3 levels ("Yes", "No", or "No internet service") for whether the individual pays for online backup
#   device_protection: A factor variable with 3 levels ("Yes", "No", or "No internet service") for whether the individual pays for device protection
#   tech_support: A factor variable with 3 levels ("Yes", "No", or "No internet service") for whether the individual pays for tech support
#   streaming_tv: A factor variable with 3 levels ("Yes", "No", or "No internet service") for whether the individual pays for TV streaming
#   streaming_movie: A factor variable with 3 levels ("Yes", "No", or "No internet service") for whether the individual pays for streaming movies
churn <- read.csv("data/Customer Churn/customer_churn.csv")

################################################################################
# Copenhagen Stroke Study
#   Jørgensen et al., Stroke, 1996, 27(10): 1765–1769
#   Data made available through Dr. Leilei Zeng (University of Waterloo)
#
# Data Columns
#   ihd: Indicator as to whether the group has IHD or not
#   prevstroke: Indicator as to whether the group has had a previous stroke
#   year: The year of study [1...10]
#   n: The total number of at risk individuals in the corresponding group, at the corresponding time
#   d: The total number of events in the corresponding group, at the corresponding time
strokes <- read.csv("data/Strokes/stroke.csv")

################################################################################
# Male Mortality Data
#   Data is coming from The Demographic Data Base, Umea University, Umeå, Sweden.
#   https://www.umu.se/enheten-for-demografi-och-aldrandeforskning/
#
# Data Columns
#   id: Personal ID
#   enter: Start of duration. Measured in years since the fortieth birthday.
#   exit: End of duration. Measured in years since the fortieth birthday.
#   event: a logical vector indicating death at end of interval.
#   birthdate: The birthdate in decimal form.
#   ses: Socio-economic status, a factor with levels lower, upper
mort <- read.csv("data/Mortality/mort.csv")

