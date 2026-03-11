setwd("/Users/abhineetmalhotra/Downloads/ONEST/")

library(readr)
library(dplyr)
library(tidyr)

data <- read_csv("Listenatscale Transcripts - Jobs Campaign  - transcripts.csv") %>%
  as_tibble()


library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

turn_level <- data %>%
  
  select(interaction_id, transcript, domain, category, language) %>%
  
  # Split transcript at Assistant/User
  mutate(turn = str_split(transcript, "(?=Assistant:|User:)")) %>%
  
  # Expand rows
  unnest(turn) %>%
  
  # Remove empty rows
  mutate(turn = str_trim(turn)) %>%
  filter(turn != "") %>%
  
  # Identify speaker
  mutate(
    speaker = case_when(
      str_starts(turn, "Assistant:") ~ "Assistant",
      str_starts(turn, "User:") ~ "User"
    )
  ) %>%
  
  # Extract text
  mutate(
    text = str_remove(turn, "^(Assistant:|User:)\\s*")
  ) %>%
  
  # Turn number within interaction
  group_by(interaction_id) %>%
  mutate(turn_number = row_number()) %>%
  ungroup() %>%
  
  select(interaction_id, turn_number, speaker, text, domain, category, language)



library(dplyr)

call_summary <- turn_level %>%
  
  group_by(interaction_id, category) %>%
  
  summarise(
    total_turns = max(turn_number),
    .groups = "drop"
  )


call_summary <- call_summary %>%
  
  mutate(
    call_status = case_when(
      total_turns == 1 ~ "Dropped",
      total_turns > 1 ~ "Engaged"
    )
  )


call_summary <- call_summary %>%
  mutate(
    turn_bucket = case_when(
      total_turns == 1 ~ "Dropped Immediately",
      total_turns <= 4 ~ "Very Short",
      total_turns <= 8 ~ "Short",
      total_turns <= 16 ~ "Medium",
      total_turns <= 24 ~ "Long",
      TRUE ~ "Very Long"
    )
  )


turn_level <- turn_level %>%
  mutate(
    text_lower = str_to_lower(text),
    
    stage = case_when(
      
      # ── Empty turns ─────────────────────────────────────────────────────────
      str_trim(text) == "" ~ "Empty Turn",
      
      # ── WhatsApp Opt-in (must be before Greeting/Intent/Self Intro to avoid misclassification) ──
      str_detect(text_lower, "send you messages on whatsapp|is it okay if we send|okay to send you messages on whatsapp") ~ "WhatsApp Opt-in",
      
      # ── Greeting ────────────────────────────────────────────────────────────
      str_detect(text_lower, "speaking with|quality and training|this call is being recorded") ~ "Greeting",
      
      # ── Self Introduction ────────────────────────────────────────────────────
      str_detect(text_lower, "i am maya|i'm maya|my name is maya|digital assistant from|ai assistant from") ~ "Self Introduction",
      
      # ── Identity Check ───────────────────────────────────────────────────────
      str_detect(text_lower, "is this .* speaking|are you .* speaking|can you pass the phone|is .* around") ~ "Identity Check",
      
      # ── Intent ──────────────────────────────────────────────────────────────
      str_detect(text_lower, "looking for a job|want.*job|post.*job|provide.*job|hire") ~ "Intent",
      
      # ── Profile (age only) ──────────────────────────────────────────────────
      str_detect(text_lower, "\\bage\\b|how old|years old") ~ "Profile",
      
      # ── Education ───────────────────────────────────────────────────────────
      str_detect(text_lower, "highest level of education|qualification|10th|12th|graduate|bachelor|nsqf") ~ "Education",
      
      # ── Experience ──────────────────────────────────────────────────────────
      str_detect(text_lower, "ever done a job before|years of work experience|work experience|fresher") ~ "Experience",
      
      # ── Job Type ────────────────────────────────────────────────────────────
      str_detect(text_lower, "type of job|full.?time|part.?time|work from home|internship") ~ "Job Type",
      
      # ── Job Role ────────────────────────────────────────────────────────────
      str_detect(text_lower, "job role|designation|position|what kind of job|what kind of work|specific preferences.*working") ~ "Job Role",
      
      # ── Vacancy Count ───────────────────────────────────────────────────────
      str_detect(text_lower, "vacanc|how many.*position|number of opening") ~ "Vacancy Count",
      
      # ── Salary ──────────────────────────────────────────────────────────────
      str_detect(text_lower, "salary|payment|stipend|ctc|per month") ~ "Salary",
      
      # ── Location District ───────────────────────────────────────────────────
      str_detect(text_lower, "district") ~ "Location District",
      
      # ── Location Area ───────────────────────────────────────────────────────
      str_detect(text_lower, "\\barea\\b|locality|city|town|village|location") ~ "Location Area",
      
      # ── WhatsApp Number Collection ──────────────────────────────────────────
      # Opt-in already caught above; this catches number request/confirmation
      str_detect(text_lower, "ten digit|10 digit|mobile number|phone number|is this your whatsapp|please share your whatsapp") ~ "WhatsApp Number",
      
      # ── Additional Needs ────────────────────────────────────────────────────
      str_detect(text_lower, "other job.related needs|training courses|accommodation|travel") ~ "Additional Needs",
      
      # ── Confirmation ────────────────────────────────────────────────────────
      str_detect(text_lower, "\\bconfirm\\b|correct|verify|is that right|please say yes to continue|please tell me what to change") ~ "Confirmation",
      
      # ── Completion ──────────────────────────────────────────────────────────
      str_detect(text_lower, "job post is now live|successfully posted|congratulations|you can now see jobs|we will send") ~ "Completion",
      
      # ── Terminal: Age Disqualified ───────────────────────────────────────────
      str_detect(text_lower, "under 18|cannot continue because you are") ~ "Age Disqualified",
      
      # ── Terminal: Wrong Number ───────────────────────────────────────────────
      str_detect(text_lower, "wrong number|sorry for the confusion.*wrong") ~ "Wrong Number",
      
      # ── Terminal: Wrong Person / Callback ────────────────────────────────────
      str_detect(text_lower, "call back when they are available|will call back when") ~ "Callback Scheduled",
      
      # ── Terminal: Opt Out ────────────────────────────────────────────────────
      # Two variants: "if you change your mind" and "to look for jobs near you"
      str_detect(text_lower, "call this number anytime|look for jobs near you|not interested") ~ "Opt Out",
      
      # ── Terminal: Network Drop ───────────────────────────────────────────────
      str_detect(text_lower, "network issue|will call you later") ~ "Network Drop",
      
      # ── Terminal: Silence Check ──────────────────────────────────────────────
      str_detect(text_lower, "are you still on call|still there|still not getting") ~ "Silence Check",
      
      # ── Language Check ───────────────────────────────────────────────────────
      str_detect(text_lower, "speak in english|which language|language you would prefer") ~ "Language Check",
      
      # ── Clarification ────────────────────────────────────────────────────────
      str_detect(text_lower, "cannot hear you|didn.t understand|trouble understanding|could you please repeat|please repeat|say that again|what did you just say|i.m not sure i understand|please clarify") ~ "Clarification",
      
      # ── Re-engagement ───────────────────────────────────────────────────────
      str_detect(text_lower, "would you still like to continue|better day and time to call|should we still continue|should we call you back") ~ "Re-engagement",
      
      # ── Call Close ───────────────────────────────────────────────────────────
      str_detect(text_lower, "have a great day|thank you for your time|i will end the call|goodbye") ~ "Call Close",
      
      # ── Callback Scheduled ───────────────────────────────────────────────────
      str_detect(text_lower, "i will call you back later|call you back later|call back later|suitable day and time") ~ "Callback Scheduled",
      
      # ── Employer Verify ─────────────────────────────────────────────────────
      str_detect(text_lower, "do you work for|representing.*works|representing.*company") ~ "Employer Verify",
      
      TRUE ~ NA_character_
    )
  ) %>%
  select(-text_lower)

turn_level %>% filter(speaker == "Assistant") %>% 
  group_by(stage) %>% summarise(total_interactions = n(), average_turn = mean(turn_number), total_sessions = n_distinct(interaction_id)) %>% 
  print(n=50)


# 1. Check Job Type over-firing — why 2x per session?
turn_level %>%
  filter(speaker == "Assistant", stage == "Job Type") %>%
  group_by(interaction_id) %>%
  summarise(n = n()) %>%
  count(n, sort = TRUE)

# 2. Split funnel by category — seeker vs provider flows will look very different
turn_level %>%
  filter(speaker == "Assistant") %>%
  group_by(category, stage) %>%
  summarise(sessions = n_distinct(interaction_id), .groups = "drop") %>%
  arrange(category, desc(sessions))

### Generating code to find unique values so that the stage column can be properly updated. 


# ── Look at a random sample of untagged Assistant turns ───────────────────────
turn_level %>%
  filter(speaker == "Assistant", is.na(stage)) %>%
  select(interaction_id, turn_number, text) %>%
  slice_sample(n = 100) %>%
  print(n = 100)

# ── Or view as a scrollable table in RStudio viewer ───────────────────────────
turn_level %>%
  filter(speaker == "Assistant", is.na(stage)) %>%
  select(interaction_id, turn_number, text) %>%
  slice_sample(n = 200) %>%
  View()

# ── Word frequency in untagged turns (quick keyword mining) ───────────────────
library(tidytext)

turn_level %>%
  filter(speaker == "Assistant", is.na(stage)) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  print(n = 50)


#### Generating code to find unique values said by the User where category is KA Job Seeker and stage is Job Role 


job_seeker_job_role <- turn_level %>%
  filter(category == "KA_Job_Seeker") %>%
  arrange(interaction_id, turn_number) %>%
  group_by(interaction_id) %>%
  mutate(next_speaker = lead(speaker), next_text = lead(text)) %>%
  ungroup() %>%
  filter(speaker == "Assistant", stage == "Job Role", next_speaker == "User") %>%
  select(interaction_id, turn_number, text, next_text) 



library(dplyr)
library(stringr)

job_seeker_job_role <- job_seeker_job_role %>%
  mutate(
    next_text_clean = next_text %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

library(dplyr)
library(stringr)

job_seeker_job_role <- job_seeker_job_role %>%
  mutate(job_category = "Other / Unclear")

library(dplyr)
library(stringr)

library(dplyr)
library(stringr)

job_seeker_job_role <- job_seeker_job_role %>%
  mutate(job_category = case_when(
    
    # ----------------------------
    # Conversation / filler
    # ----------------------------
    str_detect(next_text_clean,
               "\\b(yes|hello|hi|okay|ok|hmm|ah|uh|what|why|where|who|sorry|thank you|tell me|which one)\\b|
               ^no$|^i$|^we$|i am|i will tell|i am here|i am speaking|i am on the call") ~
      "Conversation / Acknowledgement",
    
    
    # ----------------------------
    # No clear preference
    # ----------------------------
    str_detect(next_text_clean,
               "anything|any job|any work|nothing|none|don t know|not interested|all kinds of work") ~
      "No Specific Preference",
    
    
    # ----------------------------
    # Data / computer jobs
    # ----------------------------
    str_detect(next_text_clean,
               "data entry|data analyst|data analytics|data science|data scientist|
                data operator|computer work|computer job|computer operating|
                it job|software|developer|programmer|coding|online work| computer") ~
      "Data / Computer Jobs",
    
    
    # ----------------------------
    # Engineering / technical
    # ----------------------------
    str_detect(next_text_clean,
               "engineer|engineering|civil engineer|embedded|electronics|
                cloud engineer|mechanical engineer|technical role") ~
      "Engineering / Technical",
    
    
    # ----------------------------
    # Electrical / mechanical trades
    # ----------------------------
    str_detect(next_text_clean,
               "electrician|electrical|mechanic|mechanical|fitter|welder") ~
      "Electrical / Mechanical Trades",
    
    
    # ----------------------------
    # Manufacturing / skilled trades
    # ----------------------------
    str_detect(next_text_clean,
               "machine operator|cnc|maintenance|technician|carpenter|
                welding|apprentice|factory|production") ~
      "Manufacturing / Skilled Trades",
    
    
    # ----------------------------
    # Banking / government
    # ----------------------------
    str_detect(next_text_clean,
               "bank|banking|government|govt|railway|rrb|state government|
                district collector") ~
      "Banking / Government",
    
    
    # ----------------------------
    # Defence / police
    # ----------------------------
    str_detect(next_text_clean,
               "army|police|defence|agniveer") ~
      "Defence / Police",
    
    
    # ----------------------------
    # Finance / accounting
    # ----------------------------
    str_detect(next_text_clean,
               "account|accounting|accountant|accountancy|finance|
                financial|tally|billing|cashier") ~
      "Finance / Accounting",
    
    
    # ----------------------------
    # Corporate / office jobs
    # ----------------------------
    str_detect(next_text_clean,
               "company|corporate|mnc|office work|assistant|associate|
                executive|analyst") ~
      "Corporate / Office Jobs",
    
    
    # ----------------------------
    # Sales / retail
    # ----------------------------
    str_detect(next_text_clean,
               "sales|salesman|salesperson|retail|showroom") ~
      "Sales / Retail",
    
    
    # ----------------------------
    # Customer support / BPO
    # ----------------------------
    str_detect(next_text_clean,
               "customer support|customer care|call center|bpo") ~
      "Customer Support / BPO",
    
    
    # ----------------------------
    # Education
    # ----------------------------
    str_detect(next_text_clean,
               "teacher|teaching|trainer|professor|lecturer") ~
      "Education",
    
    
    # ----------------------------
    # Healthcare
    # ----------------------------
    str_detect(next_text_clean,
               "doctor|nurse|nursing|pharmacist|lab technician|
                medical|clinical") ~
      "Healthcare",
    
    
    # ----------------------------
    # Driving / transport
    # ----------------------------
    str_detect(next_text_clean,
               "driver|driving|transport|ksrtc|truck driver") ~
      "Driving / Transport",
    
    
    # ----------------------------
    # Creative / digital
    # ----------------------------
    str_detect(next_text_clean,
               "design|designer|content creator|graphic") ~
      "Creative / Digital",
    
    
    # ----------------------------
    # Agriculture
    # ----------------------------
    str_detect(next_text_clean,
               "agriculture|farmer|farming") ~
      "Agriculture",
    
    
    # ----------------------------
    # Job type preference
    # ----------------------------
    str_detect(next_text_clean,
               "full time|part time|work from home|remote job") ~
      "Job Type Preference",
    
    
    # ----------------------------
    # Management / HR
    # ----------------------------
    str_detect(next_text_clean,
               "\\bhr\\b|human resource|manager|management|counselor") ~
      "Management / HR",
    
    
    # ----------------------------
    # Default
    # ----------------------------
    TRUE ~ "Other / Unclear"
  ))

table(job_seeker_job_role$job_category)



banking_jobs <- job_seeker_job_role %>% filter(job_category == "Banking / Government") %>% select(interaction_id)


turn_level_banking_jobs <- left_join(banking_jobs, turn_level, by=c("interaction_id"))

library(dplyr)
library(tidyr)

turn_level$interaction_new <- turn_level$interaction_id

turn_level <- turn_level %>%
  separate(interaction_id, into = c("date", "rest"), sep = "/") %>%
  separate(rest, into = c("id1", "time", "id2"), sep = "-")


library(dplyr)

turn_level <- turn_level %>%
  mutate(
    interaction_datetime = as.POSIXct(
      paste(date, time),
      format = "%Y%m%d %H:%M:%S",
      tz = "Asia/Kolkata"
    )
  )




assistant_turns <- turn_level %>%
  filter(speaker == "Assistant")


funnel <- assistant_turns %>%
  group_by(category, stage) %>%
  summarise(
    interactions = n_distinct(interaction_id),
    .groups="drop"
  )

funnel_stage <- assistant_turns %>%
  group_by(interaction_id, category) %>%
  summarise(
    greeting     = any(stage == "Greeting", na.rm = TRUE),
    intent       = any(stage == "Intent", na.rm = TRUE),
    profile      = any(stage == "Profile", na.rm = TRUE),
    job_type     = any(stage == "Job Type", na.rm = TRUE),
    job_role     = any(stage == "Job Role", na.rm = TRUE),
    salary       = any(stage == "Salary", na.rm = TRUE),
    confirmation = any(stage == "Confirmation", na.rm = TRUE),
    completion   = any(stage == "Completion", na.rm = TRUE),
    .groups = "drop"
  )

funnel <- funnel_stage %>%
  group_by(category) %>%
  summarise(
    greeting     = sum(greeting),
    intent       = sum(intent),
    profile      = sum(profile),
    job_type     = sum(job_type),
    job_role     = sum(job_role),
    salary       = sum(salary),
    confirmation = sum(confirmation),
    completion   = sum(completion),
    .groups = "drop"
  )


















library(dplyr)
library(stringr)
library(tidyr)

library(dplyr)

turn_pairs <- turn_level %>%
  
  arrange(interaction_id, turn_number) %>%
  
  group_by(interaction_id) %>%
  
  mutate(
    next_speaker = lead(speaker),
    next_text = lead(text)
  ) %>%
  
  ungroup()

qa_pairs <- turn_pairs %>%
  filter(
    speaker == "Assistant",
    next_speaker == "User",
    !is.na(stage)
  )

qa_pairs <- qa_pairs %>%
  mutate(
    extracted_value = next_text
  )

library(dplyr)
library(stringr)

# -------------------------------------------------
# 1. KEEP ONLY STAGES NEEDED
# -------------------------------------------------

structured_data <- qa_pairs %>%
  filter(stage %in% c(
    "Job Role",
    "Job Type",
    "Location Area",
    "Location District",
    "Salary",
    "Vacancy Count"
  ))

# -------------------------------------------------
# 2. STANDARDIZE TEXT
# -------------------------------------------------

clean_data <- structured_data %>%
  mutate(
    value = extracted_value %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

# -------------------------------------------------
# 3. REMOVE FILLER / CONVERSATION RESPONSES
# -------------------------------------------------

clean_data <- clean_data %>%
  mutate(
    value_type = case_when(
      str_detect(value, "^(yes|yeah|ok|okay|hmm|hello|hi)") ~ "filler",
      str_detect(value, "who are you|where are you|tell me|what") ~ "conversation",
      TRUE ~ "content"
    )
  ) %>%
  filter(value_type == "content")

# -------------------------------------------------
# 4. REMOVE EXTRA WORDS
# -------------------------------------------------

clean_data <- clean_data %>%
  mutate(
    value_clean = value %>%
      str_replace_all("\\b(job|work|madam|sir|maam|please)\\b", "") %>%
      str_squish()
  )

# -------------------------------------------------
# 5. JOB TYPE NORMALIZATION
# -------------------------------------------------

clean_data <- clean_data %>%
  mutate(
    job_type_clean = case_when(
      str_detect(value_clean, "full") ~ "Full Time",
      str_detect(value_clean, "part") ~ "Part Time",
      str_detect(value_clean, "intern") ~ "Internship",
      str_detect(value_clean, "from home|online") ~ "Work From Home",
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------
# 6. JOB ROLE NORMALIZATION
# -------------------------------------------------

clean_data <- clean_data %>%
  mutate(
    job_role_clean = case_when(
      
      # Office / data
      str_detect(value_clean, "data|computer operator|computer working|back office|office") ~ "Data / Office",
      
      # Machine / manufacturing
      str_detect(value_clean, "machine|operator|cnc|lathe") ~ "Machine Operator",
      
      # Electrical
      str_detect(value_clean, "electric|electronic") ~ "Electrician",
      
      # Driving
      str_detect(value_clean, "driver|driving|bus operator") ~ "Driver",
      
      # Sales / marketing
      str_detect(value_clean, "sales|salesman|salesperson|marketing") ~ "Sales",
      
      # Finance
      str_detect(value_clean, "account|finance|tally") ~ "Finance / Accounts",
      
      # Banking
      str_detect(value_clean, "bank") ~ "Banking",
      
      # Teaching
      str_detect(value_clean, "teacher|teaching") ~ "Teacher",
      
      # Mechanical
      str_detect(value_clean, "mechanic|automobile|garage|interior") ~ "Mechanical",
      
      # IT
      str_detect(value_clean, "software|developer|it") ~ "IT / Software",
      
      # Skilled trades
      str_detect(value_clean, "welder|welding") ~ "Welder",
      str_detect(value_clean, "carpenter") ~ "Carpenter",
      
      # HR
      str_detect(value_clean, "hr") ~ "HR",
      
      # Government roles
      str_detect(value_clean, "government|army|police|railway|upsc|psi") ~ "Government Job",
      
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------
# 7. CONVERT NUMBER WORDS
# -------------------------------------------------

number_words <- c(
  "zero"=0,"one"=1,"two"=2,"three"=3,"four"=4,"five"=5,
  "six"=6,"seven"=7,"eight"=8,"nine"=9,"ten"=10
)

clean_data <- clean_data %>%
  mutate(
    vacancy_numeric = as.numeric(str_extract(value_clean, "\\d+")),
    vacancy_numeric = ifelse(
      is.na(vacancy_numeric),
      number_words[value_clean],
      vacancy_numeric
    )
  )

# -------------------------------------------------
# 8. EXTRACT SALARY
# -------------------------------------------------

clean_data <- clean_data %>%
  mutate(
    salary_numeric = as.numeric(str_extract(value_clean, "\\d{4,6}"))
  )

# -------------------------------------------------
# 9. CLEAN LOCATION FIELDS
# -------------------------------------------------

clean_data <- clean_data %>%
  mutate(
    location_district_clean = ifelse(stage == "Location District", str_to_title(value_clean), NA),
    location_area_clean = ifelse(stage == "Location Area", str_to_title(value_clean), NA)
  )

# -------------------------------------------------
# 10. KEEP LAST VALID ANSWER PER STAGE
# -------------------------------------------------

clean_data <- clean_data %>%
  group_by(interaction_id, stage) %>%
  slice_tail(n = 1) %>%
  ungroup()

ka_data <- clean_data %>%
  filter(category %in% c("KA_Job_Seeker", "KA_Job_Provider"))


ka_roles <- ka_data %>%
  filter(!is.na(job_role_clean)) %>%
  select(interaction_id, category, job_role_clean) %>%
  distinct()


role_counts <- ka_roles %>%
  count(category, job_role_clean)

seekers <- role_counts %>%
  filter(category == "KA_Job_Seeker") %>%
  rename(seeker_count = n)

providers <- role_counts %>%
  filter(category == "KA_Job_Provider") %>%
  rename(provider_count = n)

role_matches <- seekers %>%
  inner_join(providers, by = "job_role_clean")

