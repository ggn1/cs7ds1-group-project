# CLEAN UP PACKAGES (DETATCH ALL)

# Get a list of loaded packages
loaded_packages <- search()

# Detach all loaded packages
for (pkg in loaded_packages) {
  if ("package:" %in% pkg) {
    detach(pkg, unload = TRUE)
  }
}