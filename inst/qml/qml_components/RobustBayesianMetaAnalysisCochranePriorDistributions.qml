//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

DropDown
{
	label:					qsTr("Medical subfield")
	id:						cochranePriorDropdown
	name:					"priorDistributionsEffectSizeAndHeterogeneityMedicineSubfield"
	fieldWidth:				150 * preferencesModel.uiScale
	property string effectSizeMeasure:	"SMD"
	values: 
		if (effectSizeMeasure === "SMD" || effectSizeMeasure === "fishersZ")
		[
			{ label: qsTr("General"), value: "general" },
			{ label: qsTr("Acute Respiratory Infections"), value: "acuteRespiratoryInfections" },
			{ label: qsTr("Airways"), value: "airways" },
			{ label: qsTr("Anaesthesia"), value: "anaesthesia" },
			{ label: qsTr("Back and Neck"), value: "backAndNeck" },
			{ label: qsTr("Bone, Joint and Muscle Trauma"), value: "boneJointAndMuscleTrauma" },
			{ label: qsTr("Colorectal"), value: "colorectal" },
			{ label: qsTr("Common Mental Disorders"), value: "commonMentalDisorders" },
			{ label: qsTr("Consumers and Communication"), value: "consumersAndCommunication" },
			{ label: qsTr("Cystic Fibrosis and Genetic Disorders"), value: "cysticFibrosisAndGeneticDisorders" },
			{ label: qsTr("Dementia and Cognitive Improvement"), value: "dementiaAndCognitiveImprovement" },
			{ label: qsTr("Developmental, Psychosocial and Learning Problems"), value: "developmentalPsychosocialAndLearningProblems" },
			{ label: qsTr("Drugs and Alcohol"), value: "drugsAndAlcohol" },
			{ label: qsTr("Effective Practice and Organisation of Care"), value: "effectivePracticeAndOrganisationOfCare" },
			{ label: qsTr("Emergency and Critical Care"), value: "emergencyAndCriticalCare" },
			{ label: qsTr("ENT"), value: "ent" },
			{ label: qsTr("Eyes and Vision"), value: "eyesAndVision" },
			{ label: qsTr("Gynaecological, Neuro-oncology and Orphan Cancer"), value: "gynaecologicalNeuroOncologyAndOrphanCancer" },
			{ label: qsTr("Gynaecology and Fertility"), value: "gynaecologyAndFertility" },
			{ label: qsTr("Heart"), value: "heart" },
			{ label: qsTr("Hepato-Biliary"), value: "hepatoBiliary" },
			{ label: qsTr("HIV/AIDS"), value: "hivAids" },
			{ label: qsTr("Hypertension"), value: "hypertension" },
			{ label: qsTr("Incontinence"), value: "incontinence" },
			{ label: qsTr("Infectious Diseases"), value: "infectiousDiseases" },
			{ label: qsTr("Inflammatory Bowel Disease"), value: "inflammatoryBowelDisease" },
			{ label: qsTr("Injuries"), value: "injuries" },
			{ label: qsTr("Kidney and Transplant"), value: "kidneyAndTransplant" },
			{ label: qsTr("Metabolic and Endocrine Disorders"), value: "metabolicAndEndocrineDisorders" },
			{ label: qsTr("Methodology"), value: "methodology" },
			{ label: qsTr("Movement Disorders"), value: "movementDisorders" },
			{ label: qsTr("Musculoskeletal"), value: "musculoskeletal" },
			{ label: qsTr("Neonatal"), value: "neonatal" },
			{ label: qsTr("Oral Health"), value: "oralHealth" },
			{ label: qsTr("Pain, Palliative and Supportive Care"), value: "painPalliativeAndSupportiveCare" },
			{ label: qsTr("Pregnancy and Childbirth"), value: "pregnancyAndChildbirth" },
			{ label: qsTr("Public Health"), value: "publicHealth" },
			{ label: qsTr("Schizophrenia"), value: "schizophrenia" },
			{ label: qsTr("Sexually Transmitted Infections"), value: "sexuallyTransmittedInfections" },
			{ label: qsTr("Skin"), value: "skin" },
			{ label: qsTr("Stroke"), value: "stroke" },
			{ label: qsTr("Tobacco Addiction"), value: "tobaccoAddiction" },
			{ label: qsTr("Upper GI and Pancreatic Diseases"), value: "upperGiAndPancreaticDiseases" },
			{ label: qsTr("Urology"), value: "urology" },
			{ label: qsTr("Vascular"), value: "vascular" },
			{ label: qsTr("Work"), value: "work" },
			{ label: qsTr("Wounds"), value: "wounds" }
		]
		else if (effectSizeMeasure === "logOR" || effectSizeMeasure === "logRR" || effectSizeMeasure === "RD")
		[
			{ label: qsTr("General"), value: "general" },
			{ label: qsTr("Acute Respiratory Infections"), value: "acuteRespiratoryInfections" },
			{ label: qsTr("Airways"), value: "airways" },
			{ label: qsTr("Anaesthesia"), value: "anaesthesia" },
			{ label: qsTr("Back and Neck"), value: "backAndNeck" },
			{ label: qsTr("Bone, Joint and Muscle Trauma"), value: "boneJointAndMuscleTrauma" },
			{ label: qsTr("Breast Cancer"), value: "breastCancer" },
		//	{ label: qsTr("Childhood Cancer"), value: "childhoodCancer" }, // (there) is no heterogeneity prior distribution for this subfield
			{ label: qsTr("Chronic Pain"), value: "chronicPain" },
			{ label: qsTr("Cirrhosis and Liver Transplantation"), value: "cirrhosisAndLiverTransplantation" },
			{ label: qsTr("Cognitive Impairment and Dementia"), value: "cognitiveImpairmentAndDementia" },
			{ label: qsTr("Colorectal Cancer"), value: "colorectalCancer" },
			{ label: qsTr("Colorectal"), value: "colorectal" },
			{ label: qsTr("Common Mental Disorders"), value: "commonMentalDisorders" },
			{ label: qsTr("Consumers and Communication"), value: "consumersAndCommunication" },
			{ label: qsTr("Cystic Fibrosis and Genetic Disorders"), value: "cysticFibrosisAndGeneticDisorders" },
			{ label: qsTr("Dementia and Cognitive Improvement"), value: "dementiaAndCognitiveImprovement" },
			{ label: qsTr("Developmental, Psychosocial and Learning Problems"), value: "developmentalPsychosocialAndLearningProblems" },
			{ label: qsTr("Drugs and Alcohol"), value: "drugsAndAlcohol" },
			{ label: qsTr("Effective Practice and Organisation of Care"), value: "effectivePracticeAndOrganisationOfCare" },
			{ label: qsTr("Emergency and Critical Care"), value: "emergencyAndCriticalCare" },
			{ label: qsTr("ENT"), value: "ent" },
			{ label: qsTr("Epilepsy"), value: "epilepsy" },
			{ label: qsTr("Eyes and Vision"), value: "eyesAndVision" },
			{ label: qsTr("Fertility Regulation"), value: "fertilityRegulation" },
			{ label: qsTr("Gut"), value: "gut" },
			{ label: qsTr("Gynaecological, Neuro-oncology and Orphan Cancer"), value: "gynaecologicalNeuroOncologyAndOrphanCancer" },
			{ label: qsTr("Gynaecology and Fertility"), value: "gynaecologyAndFertility" },
			{ label: qsTr("Haematology"), value: "haematology" },
			{ label: qsTr("Heart"), value: "heart" },
			{ label: qsTr("Heart; Vascular"), value: "heartVascular" },
			{ label: qsTr("Hepato-Biliary"), value: "hepatoBiliary" },
			{ label: qsTr("HIV/AIDS"), value: "hivAids" },
			{ label: qsTr("Hypertension"), value: "hypertension" },
			{ label: qsTr("Incontinence"), value: "incontinence" },
			{ label: qsTr("Infectious Diseases"), value: "infectiousDiseases" },
			{ label: qsTr("Injuries"), value: "injuries" },
			{ label: qsTr("Kidney and Transplant"), value: "kidneyAndTransplant" },
			{ label: qsTr("Lung Cancer"), value: "lungCancer" },
			{ label: qsTr("Metabolic and Endocrine Disorders"), value: "metabolicAndEndocrineDisorders" },
			{ label: qsTr("Methodology"), value: "methodology" },
			{ label: qsTr("Movement Disorders"), value: "movementDisorders" },
			{ label: qsTr("Multiple Sclerosis and Rare Diseases of the CNS"), value: "multipleSclerosisAndRareDiseasesOfTheCNS" },
			{ label: qsTr("Musculoskeletal"), value: "musculoskeletal" },
			{ label: qsTr("Neonatal"), value: "neonatal" },
			{ label: qsTr("Neuromuscular"), value: "neuromuscular" },
			{ label: qsTr("Oral Health"), value: "oralHealth" },
			{ label: qsTr("Pain, Palliative and Supportive Care"), value: "painPalliativeAndSupportiveCare" },
			{ label: qsTr("Pregnancy and Childbirth"), value: "pregnancyAndChildbirth" },
			{ label: qsTr("Schizophrenia"), value: "schizophrenia" },
			{ label: qsTr("Sexually Transmitted Infections"), value: "sexuallyTransmittedInfections" },
			{ label: qsTr("Skin"), value: "skin" },
			{ label: qsTr("Stroke"), value: "stroke" },
			{ label: qsTr("Tobacco Addiction"), value: "tobaccoAddiction" },
			{ label: qsTr("Urology"), value: "urology" },
			{ label: qsTr("Vascular"), value: "vascular" },
			{ label: qsTr("Work"), value: "work" },
			{ label: qsTr("Wounds"), value: "wounds" }
		]
		else if (effectSizeMeasure === "logHR")
		[
			{ label: qsTr("General"), value: "general" }
		]
}