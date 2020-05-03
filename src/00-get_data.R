library(qualtRics)

#Get Survey from Quatrics with API
seas_survey_id = 'SV_ekZqvDRASUWJnnv'
df_seas = fetch_survey(surveyID = seas_survey_id,
                    force_request = TRUE,
                    time_zone = "America/Chicago")

#Save as RDS to keep column labels
saveRDS(df_seas, "raw/seas_one.rds")
