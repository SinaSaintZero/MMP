# 🧠 MMP: A Hybrid AI Framework for Diagnosing Concept Drifts in Patient Pathways

**Minuscule Movement of business Processes (MMP)** is a hybrid AI platform designed to detect and explain deviations in patient pathways by integrating domain knowledge with process mining techniques. Inspired by NASA’s exoplanet detection strategy, MMP introduces a novel approach to diagnose concept drifts through artificial model generation and semantic reasoning.

** This is a test page created to help readers visualize the work and code structure. If you have any questions please contact
sina.namakiaraghi@uttop.fr

## 📌 Key Features

- 🔍 **Explainable Business Process Drift Diagnosis**: Goes beyond detection to uncover root causes of process deviations.
- 🧠 **Hybrid AI Architecture**: Combines symbolic (ontology-driven) and sub-symbolic (process mining) intelligence.
- 🏥 **Healthcare Focus**: Tailored for clinical pathway analysis and hospital process optimization.
- 🧬 **ProDIST Algorithm**: Measures semantic distance between process models to identify the most probable cause of drift.
- 🧩 **Domain Knowledge Integration**: Embeds expert-defined Potential Assignable Causes (PACs) into process models.

## 📁 Repository Structure
MMP/

src/ – Core algorithms and model generation logic

data/ – Sample event logs and reference models

ontology/ – Domain knowledge and semantic definitions

docs/ – Manuscript, figures, and supplementary materials

tests/ – Unit tests and validation scripts



## 🚀 Getting Started

You need to install the R programming language and the relevant libraries 

1. **Clone the repository**:
   ```bash
   git clone https://github.com/SinaSaintZero/MMP.git
   cd MMP

   📚 Methodology Overview
MMP operates in three stages:

Reference Model (RM): Defined by domain experts to represent ideal patient pathways.

Generated Models (GM): Artificial models created by injecting PACs into RM.

Descriptive Model (DM): Discovered from real-world event logs using Stable Heuristic Miner.

The ProDIST algorithm compares DM to each GM and identifies the PAC most likely responsible for the deviation.

🧠 Scientific Inspiration
MMP draws inspiration from NASA’s minuscule movement method for exoplanet detection. Just as gravitational anomalies suggest hidden celestial bodies, process deviations suggest hidden assignable causes. MMP formalizes this analogy into a diagnostic framework for healthcare processes.

🧪 Experimentation
A real-life hospital case study demonstrates MMP’s ability to:

Detect structural drifts in patient pathways

Explain deviations using embedded domain knowledge

Provide actionable insights for clinical process improvement

📄 License
This project is licensed under the MIT License. See the LICENSE file for details.

👥 Authors
Dr. Sina Namaki Araghi (UTTOP, ENIT)

Prof. Dr. Frederick Benaben, Franck Fontanili, Elyes Lamine (IMT Mines Albi)

Dr. Jalal Possik (Université Catholique de Lille)

Prof.Dr. Adriano O. Solis (York University)

Prof.Dr. Mohammed-Hedi Karray (UTTOP, ENIT)
