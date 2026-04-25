# ==========================================================
# --- AUTOMATED GITHUB DEPLOYMENT ---
# ==========================================================
print("--- MAP PREPARATION COMPLETE ---")
print("Preparing to push updates to GitHub...")

# 1. Ask the user for a custom commit message in the R Console
user_msg <- readline(prompt = "Enter a commit message or press Enter to just use the timestamp: ")

# 2. Generate the current timestamp
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# 3. Combine them into the final commit message
if (trimws(user_msg) == "") {
  commit_msg <- paste("Auto-update map data:", timestamp)
} else {
  commit_msg <- paste(user_msg, "-", timestamp)
}

print(paste("Committing with message:", commit_msg))

# 4. Execute the Git commands
# Add the specific map folders (prevents accidentally committing unrelated R scripts if you don't want to)
system("git add .") 

system('git config user.email "alenalexpathisseril@gmail.com"')
system('git config user.name "AlenAlex3112"')

# Commit the changes using sprintf to safely wrap the message in quotes
system(sprintf('git commit -m "%s"', commit_msg))

# Push to GitHub
print("Pushing to GitHub Pages...")
push_status <- system("git push")

# 5. Check if it worked
if (push_status == 0) {
  print("==========================================================")
  print(" SUCCESS! Your map has been pushed and will be live soon.")
  print("==========================================================")
} else {
  print("==========================================================")
  print(" ERROR: Git push failed. Please check your console output.")
  print("==========================================================")
}