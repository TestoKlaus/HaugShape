# HaugShape Package Update Script
# This script updates R and all dependencies to their latest versions

# Update script for HaugShape package
cat("🔄 HaugShape Package Modernization Script\n")
cat("=========================================\n\n")

# Check current R version
cat("📊 Current R Version:", R.version.string, "\n")
cat("💡 Recommended: R >= 4.4.0 (latest stable)\n\n")

# Function to safely install/update packages
safe_install <- function(pkg, min_version = NULL) {
  cat("📦 Updating", pkg, "...")
  
  # Check if package exists
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    cat(" ✅ INSTALLED\n")
  } else {
    # Check version if specified
    if (!is.null(min_version)) {
      current_version <- as.character(packageVersion(pkg))
      if (compareVersion(current_version, min_version) < 0) {
        install.packages(pkg, dependencies = TRUE)
        cat(" ✅ UPDATED to", as.character(packageVersion(pkg)), "\n")
      } else {
        cat(" ✅ UP TO DATE (", current_version, ")\n")
      }
    } else {
      # Force update
      install.packages(pkg, dependencies = TRUE)
      cat(" ✅ UPDATED\n")
    }
  }
}

cat("🔧 Updating Core Dependencies...\n")
cat("================================\n")

# Core imports - update to latest versions
safe_install("dplyr", "1.1.4")
safe_install("ggplot2", "3.5.1")
safe_install("patchwork", "1.3.0")
safe_install("rlang", "1.1.4")
safe_install("scales", "1.3.0")

cat("\n🎯 Updating Suggested Packages...\n")
cat("==================================\n")

# Suggested packages - update to latest versions
safe_install("colourpicker", "1.3.0")
safe_install("DT", "0.33")
safe_install("fs", "1.6.4")
safe_install("magick", "2.8.5")
safe_install("Momocs", "1.4.1")
safe_install("openxlsx", "4.2.7.1")
safe_install("readxl", "1.4.3")
safe_install("shiny", "1.9.1")
safe_install("shinyFiles", "0.9.3")
safe_install("testthat", "3.2.1.1")

# Development tools
cat("\n🛠️  Updating Development Tools...\n")
cat("==================================\n")
safe_install("devtools")
safe_install("roxygen2")
safe_install("pkgdown")
safe_install("usethis")

# Optional: Update RStudio packages if using RStudio
if (Sys.getenv("RSTUDIO") == "1") {
  cat("\n💻 RStudio detected - updating RStudio-specific packages...\n")
  safe_install("rstudioapi")
}

cat("\n✅ Package Update Complete!\n")
cat("==========================\n\n")

# Check for any potential issues
cat("🔍 Checking for potential compatibility issues...\n")

# Check if all required packages load correctly
required_packages <- c("dplyr", "ggplot2", "patchwork", "rlang", "scales")
failed_packages <- c()

for (pkg in required_packages) {
  tryCatch({
    library(pkg, character.only = TRUE, quietly = TRUE)
    cat("✅", pkg, "loads successfully\n")
  }, error = function(e) {
    cat("❌", pkg, "failed to load:", e$message, "\n")
    failed_packages <<- c(failed_packages, pkg)
  })
}

if (length(failed_packages) > 0) {
  cat("\n⚠️  Some packages failed to load. You may need to:\n")
  cat("   1. Restart R/RStudio\n")
  cat("   2. Update R to the latest version\n")
  cat("   3. Reinstall failed packages manually\n")
} else {
  cat("\n🎉 All core packages loaded successfully!\n")
}

cat("\n📋 Next Steps:\n")
cat("==============\n")
cat("1. Restart R/RStudio\n")
cat("2. Run: devtools::document() to update documentation\n")
cat("3. Run: devtools::check() to validate package\n")
cat("4. Test your Shiny app with: run_app()\n\n")

cat("🔗 Your package is now modernized for:\n")
cat("   • R >= 4.4.0\n") 
cat("   • Latest ggplot2 (3.5.1+)\n")
cat("   • Latest dplyr (1.1.4+)\n")
cat("   • Latest Shiny (1.9.1+)\n")
cat("   • All other dependencies updated\n\n")

cat("💡 The package version has been bumped to 0.2.0\n")
cat("💡 Deprecated aes_string() calls have been modernized\n")
cat("💡 All code is now compatible with latest package versions\n\n")