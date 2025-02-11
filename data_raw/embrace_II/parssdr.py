#%%
from difflib import get_close_matches
import json

# Function to replace keys with closest matching new keys
def replace_keys_with_closest(json_data, new_keys_list):
    new_json_data = {}
    replacements = []
    for key, value in json_data.items():
        closest_key = get_close_matches(key.lower(), new_keys_list, n=1)
        if closest_key:
            new_json_data[closest_key[0]] = value
            replacements.append((key, closest_key[0]))
        else:
            new_json_data[key] = value  # Keep the original key if no match is found
    return new_json_data, replacements

json_data = {
    "VagMorbStudyValue_TR": {"0": "No", "1": "Yes"},
    "VagIQStudyVisible_TR": {"0": "No", "1": "Yes"},
    "CoMorbidity_TR": {"0": "No", "1": "Yes"},
    "MyocardialInfarction_TR": {"0": "No", "1": "Yes"},
    "CongestiveCardiacFailure_TR": {"0": "No", "1": "Yes"},
    "PeripheralVascularDisease_TR": {"0": "No", "1": "Yes"},
    "CerebrovascularDisease_TR": {"0": "No", "1": "Yes"},
    "Dementia_TR": {"0": "No", "1": "Yes"},
    "ChronicObtructivePulmDisease_TR": {"0": "No", "1": "Yes"},
    "RheumatologicDisease_TR": {"0": "No", "1": "Yes"},
    "PepticUlcerDisease_TR": {"0": "No", "1": "Yes"},
    "MildLiverDisease_TR": {"0": "No", "1": "Yes"},
    "DiabetesWithoutChronicComplications_TR": {"0": "No", "1": "Yes"},
    "DiabetesWithChronicComplications_TR": {"0": "No", "1": "Yes"},
    "HemiplegiaOrParaplegia_TR": {"0": "No", "1": "Yes"},
    "RenalDisease_TR": {"0": "No", "1": "Yes"},
    "ModerateSevereLiverDisease_TR": {"0": "No", "1": "Yes"},
    "AIDSHIV_TR": {"0": "No", "1": "Yes"},
    "Smoker_TR": {"0": "No", "1": "Yes", "2": "Previous smoker", "9": "NA"},
    "PreviousPelvicAbdSurgery_TR": {"0": "No", "1": "Yes"},
    "Partner_TR": {"0": "No partner", "1": "Partner", "9": "Unknown"},
    "Menopause_TR": {"0": "Premenopausal", "1": "Postmenopausal", "9": "Unknown"},
    "VaginalBleeding_TR": {"0": "No", "1": "Moderate", "2": "Transfusion required"},
    "Pain_TR": {"0": "No", "1": "Non-opoid medication", "2": "Opoids required"},
    "OtherSymptoms_TR": {"0": "No", "1": "Yes"},
    "WHOScore_TR": {"0": "PS0", "1": "PS1", "2": "PS2", "3": "PS3", "4": "PS4"},
    "GynLeftParametrium_TR": {"0": "Not involved", "1": "Proximal", "2": "Distal", "3": "To pelvic wall"},
    "GynRightParametrium_TR": {"0": "Not involved", "1": "Proximal", "2": "Distal", "3": "To pelvic wall"},
    "GynVagina_TR": {"0": "Not involved", "1": "Upper 1/3", "2": "Middle 1/3", "3": "Lower 1/3"},
    "GynVaginaAnterior_TR": {"0": "No", "1": "Yes"},
    "GynVaginaPosterior_TR": {"0": "No", "1": "Yes"},
    "GynVaginaLeftLateral_TR": {"0": "No", "1": "Yes"},
    "GynVaginaRightLateral_TR": {"0": "No", "1": "Yes"},
    "GynBladderCystoscopy_TR": {"0": "Not involved", "1": "Bullous edema", "2": "Mucosa involved", "9": "Not done"},
    "GynRectumExploration_TR": {"0": "Not involved", "1": "Palpable impression", "2": "Tumor directly palpable", "9": "Not done"},
    "GynRectumEndoscopy_TR": {"0": "Not involved", "1": "Impression but mucosa intact", "2": "Mucosa involved", "9": "Not done"},
    "Biopsy_TR": {"0": "No", "1": "Yes"},
    "Cone_TR": {"0": "No", "1": "Yes"},
    "LaparascopicStaging_TR": {"0": "No", "1": "Yes"},
    "DiagnosticOther_TR": {"0": "No", "1": "Yes"},
    "HistopathologicalType_TR": {"1": "SQ", "2": "AC", "3": "AdSq"},
    "DegreeOfDifferentiation_TR": {"1": "High", "2": "Moderate", "3": "Low", "9": "NA"},
    "LVI_TR": {"0": "No", "1": "Yes", "9": "NA"},
    "BloodTestHgbUnit_TR": {"1": "mmol/L", "2": "g/dL"},
    "BloodTestCreatinineUnit_TR": {"1": "µmol/L", "2": "mg/dL"},
    "FDGPETCTWholeBody_TR": {"0": "No", "1": "Yes"},
    "CTalone_TR": {"1": "pelvis, abdomen, chest", "2": "pelvis, abdomen", "3": "chest"},
    "MRICervix1_TR": {"0": "No tumor", "1": "Exophytic tumour", "2": "Endophytic tumour", "3": "Exo- and endophytic tumour"},
    "MRICervix2_TR": {"0": "No necrosis", "1": "Necrosis"},
    "MRILeftParametrium_TR": {"0": "Not involved", "1": "Proximal", "2": "Distal", "3": "To pelvic wall"},
    "MRIRightParametrium_TR": {"0": "Not involved", "1": "Proximal", "2": "Distal", "3": "To pelvic wall"},
    "MRITumorInfiltrationType_TR": {"1": "Predominant expansive", "2": "Predominant infiltrating tumours"},
    "MRICorpusUteri_TR": {"0": "Not involved", "1": "Lower", "2": "Mid", "3": "Upper"},
    "MRIVagina_TR": {"0": "Not involved", "1": "Upper 1/3", "2": "Middle 1/3", "3": "Lower 1/3"},
    "MRIBladder_TR": {"0": "Not involved", "1": "Bladder wall infiltration"},
    "MRIRectum_TR": {"0": "Not involved", "1": "Mesorectum invaded", "2": "Rectal wall invaded"},
    "MRIContrastAgentType_TR": {"1": "Dotarem", "2": "Gadovist", "3": "Clariscan", "9": "Other"},
    "MRIIncludingT2_TR": {"0": "No", "1": "Yes"},
    "Hydronephrosis_TR": {"0": "No", "1": "Left", "2": "Right", "3": "Bilat"},
    "PathologicalNodesPresent_TR": {"0": "No", "1": "Yes"},
}

new_keys_list = [
    "id_sta_d", "centre_id_sta_d", "general_comments_sta_d", "co_morbidity_sta_d",
    "myocardial_infarction_sta_d", "congestive_cardiac_failure_sta_d", "peripheral_vascular_disease_sta_d",
    "cerebrovascular_disease_sta_d", "dementia_sta_d", "chronic_obtructive_pulm_disease_sta_d",
    "rheumatologic_disease_sta_d", "peptic_ulcer_disease_sta_d", "mild_liver_disease_sta_d",
    "diabetes_without_chronic_complications_sta_d", "diabetes_with_chronic_complications_sta_d",
    "hemiplegia_or_paraplegia_sta_d", "renal_disease_sta_d", "moderate_severe_liver_disease_sta_d",
    "aidshiv_sta_d", "charlson_comorbidity_index_sta_d", "smoker_sta_d", "previous_pelvic_abd_surgery_sta_d",
    "previous_pelvic_abd_surgery_text_sta_d", "partner_sta_d", "menopause_sta_d", "vaginal_bleeding_sta_d",
    "pain_sta_d", "other_symptoms_sta_d", "other_symptoms_text_sta_d", "who_score_sta_d", "height_sta_d",
    "weight_sta_d", "bmi_sta_d", "figo_stage_sta_d", "gyn_tumor_width_sta_d", "gyn_tumor_thickness_sta_d",
    "gyn_left_parametrium_sta_d", "gyn_right_parametrium_sta_d", "gyn_vagina_sta_d", "gyn_vagina_anterior_sta_d",
    "gyn_vagina_posterior_sta_d", "gyn_vagina_left_lateral_sta_d", "gyn_vagina_right_lateral_sta_d",
    "gyn_vagina_max_distal_extension_fornix_sta_d", "gyn_bladder_cystoscopy_sta_d", "gyn_rectum_exploration_sta_d",
    "gyn_rectum_endoscopy_sta_d", "biopsy_sta_d", "cone_sta_d", "laparascopic_staging_sta_d", "diagnostic_other_sta_d",
    "diagnostic_other_text_sta_d", "histopathological_type_sta_d", "degree_of_differentiation_sta_d", "lvi_sta_d",
    "fdgpetct_whole_body_sta_d", "c_talone_sta_d", "mri_tumor_width_sta_d", "mri_tumor_height_sta_d",
    "mri_tumor_thickness_sta_d", "mri_cervix1_sta_d", "mri_cervix2_sta_d", "mri_necrosis_diameter_sta_d",
    "mri_left_parametrium_sta_d", "mri_right_parametrium_sta_d", "mri_tumor_infiltration_type_sta_d",
    "mri_corpus_uteri_sta_d", "mri_vagina_sta_d", "mri_bladder_sta_d", "mri_rectum_sta_d", "hydronephrosis_sta_d",
    "pathological_nodes_sta_d", "created_sta_d", "created_by_sta_d", "last_updated_sta_d", "last_updated_by_sta_d",
    "deleted_date_sta_d", "deleted_by_sta_d", "deleted_sta_d", "submitted_sta_d", "gyn_tumor_height_sta_d",
    "mri_contrast_agent_amount_sta_d", "mri_contrast_agent_type_sta_d", "mri_contrast_agent_type_other_text_sta_d",
    "mri_including_t2_sta_d", "tnmt_stage_sta_d", "tnmn_stage_sta_d", "tnmm_stage_sta_d", "id1_sta_d", "centre_id1_sta_d",
    "embrace_id1_sta_d", "initials_sta_d", "birth_date_sta_d", "year_of_birth_sta_d", "registration_date_sta_d",
    "created1_sta_d", "created_by1_sta_d", "last_updated1_sta_d", "last_updated_by1_sta_d", "deleted_date1_sta_d",
    "deleted_by1_sta_d", "deleted1_sta_d", "submitted1_sta_d", "mri_max_parametrium_sta_d", "gyn_max_parametrium_sta_d",
    "mri_max_tumor_dimension_sta_d", "gyn_max_tumor_dimension_sta_d", "max_tumor_dimension_sta_d"
]


# Replace keys in the original JSON data
new_json_data, replacements = replace_keys_with_closest(json_data, new_keys_list)

# Display the updated JSON data and the replacements
print(json.dumps(new_json_data, indent=4))
print("\nReplacements:")
for old_key, new_key in replacements:
    print(f"{old_key} was replaced with {new_key}")
# %%
