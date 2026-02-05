# Code-It 
Repository for Code It! app. Web app available at <http://codeitapp.org>.  

Fully an R Shiny application.  
   
Code It! streamlines automated qualitative coding by combining keyword-based classifiers with statistical validation. Keyword-based classifiers allow for fair and transparent automated coding processes. Build, train, and validate your coding system with confidence using perfect sampling methodology.  

# Overview
Code It! streamlines automated qualitative coding by combining keyword-based classifiers with statistical validation. Keyword-based classifiers allow for fair and transparent automated coding processes. Build, train, and validate your coding system with confidence using perfect sampling methodology.  

# Workflow
1. Upload Data – Import your CSV or Excel file and select the column with data
2. Create Code – Validate one code at a time. Define your code with a name, definition, and examples.
3. Create Classifiers – Add keywords or regex patterns to identify your code.
4. Training – Review examples and refine your classifier. Keep track of Cohen's Kappa, False Discovery Rate, and False Omission Rate.
5. Validation – Achieve κ ≥ 0.80 through perfect sampling cycles.
6. Code Dataset – Apply your validated classifier to all data and download final metrics.  

# Perfect Sampling Validation
The app uses a cycle-based perfect validation approach (Shaffer & Cai's 2024)

-Calculates required sample size (Cai's N) based on your classifier's performance  
-Tracks consecutive perfect agreements between you and the classifier  
-Any disagreement ends the cycle, moves the item to training, and prompts classifier refinement  
-Validation is complete when you achieve the required number of consecutive agreements (κ > 0.80, α = 0.025)  

This ensures statistical confidence before coding your full dataset.  

# Acknowledgments and References
Inspired by the Epistemic Analytics Lab and developed with assistance from Claude's Sonnet v4.5 LLM model.  

Shaffer, D.W. & Cai, Z. (2024). Perfect Sampling.  

Arastoopour Irgens, G. & Eagan, B. (2023). The Foundations and Fundamentals of Quantitative Ethnography  

Shaffer, D.W. & Ruis, A.R. (2021). How We Code.  

Eagan, B. & colleagues. (2015). Can We Rely on IRR?  

# Author
Golnaz Arastoopour Irgens

# Recommended Citation
If you use Code It! in your research, please cite:  

Arastoopour Irgens, G., Cai, Z., Eagan, B., Marquart, C., Ruis, A.R., Tan, Y., & Williamson Shaffer, D. (2025). Code It!: A web-based application for developing and validating automated qualitative coding systems. [URL]

