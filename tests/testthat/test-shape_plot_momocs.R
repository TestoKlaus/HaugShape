# Example data
df <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  group = sample(c("A", "B", "C"), 100, replace = TRUE)
)

# Test case: Plot with convex hulls for groups A, B, and C
shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B", "C"),
           hull_fill = c("red", "green", "blue"), hull_alpha = c(0.2, 0.3, 0.5),
           point_color = c("red", "green", "blue"), point_fill = "black", point_shape = 21,  # Specify point fill
           point_size = 4, title = "Example Plot", x_label = "PC1 (....%)", y_label = "PC2 (....%)",
           x_label_adjust = 0, show_hulls = TRUE)

# Test case: Plot with no convex hulls (only scatter points)
shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B", "C"),
           hull_fill = c("red", "green", "blue"), hull_alpha = c(0.2, 0.3, 0.5),
           point_color = c("red", "green", "blue"), point_fill = "black", point_shape = 21,  # Specify point fill
           point_size = 4, title = "Example Plot (No Hulls)", x_label = "PC1 (....%)", y_label = "PC2 (....%)",
           x_label_adjust = 0, show_hulls = FALSE)

# Test case: Plot with convex hulls for only groups A and B
shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B"),
           hull_fill = c("red", "green"), hull_alpha = c(0.2, 0.3),
           point_color = c("red", "green","blue"), point_fill = "black", point_shape = 21,  # Specify point fill
           point_size = 4, title = "Example Plot (Groups A and B)", x_label = "PC1 (....%)", y_label = "PC2 (....%)",
           x_label_adjust = 0)

# Test case: Display hulls for groups A and C only
shape_plot(df, "x", "y", group_col = "group", group_vals = c("A", "B", "C"),
           hull_fill = c("red", "green", "blue"), hull_alpha = c(0.2, 0.3, 0.5),
           point_color = c("red", "green", "blue"), point_fill = "black", point_shape = 21,  # Specify point fill
           point_size = 4, title = "Example Plot (Hulls for A and C)", x_label = "PC1 (....%)", y_label = "PC2 (....%)",
           x_label_adjust = 0, show_hulls = TRUE, show_hull_for_groups = c("A", "C"))




# Test the shape_plot_with_pca_shapes function

# Simulate example data for the PCA
example_pca_data <- matrix(rnorm(200), ncol = 2)
example_pca <- prcomp(example_pca_data)

# Example file path to load the shapes (replace with your actual file path)
lf_H <- list.files("G:/Meine Ablage/01_LMU/4. Semester/02_DZG Tagung/01_Head", full.names = TRUE)

# Load the shapes into a Coo object
H <- import_jpg(lf_H) %>% Out()

#efourier
H.e <- efourier(H,norm = FALSE)

#pca
H.pca <- PCA(H.e)

# Test the shape_plot_momocs function
test_plot <- shape_plot_momocs(
  pca_result = H.pca,
  coo_object = H,  # Coo object containing the shapes
  pc_x = 1,
  pc_y = 2,
  shape_size = 0.03,  # Adjust the size of shapes
  shift_distance = 0.07,  # Set the shift distance for shapes
  vertical_offset = -5,  # Set the vertical offset for shapes
  horizontal_offset = 0,  # Adjust horizontal position of shapes
  tick_length = 0.01,  # Adjust the tick length
  arrow_shift = 0.1,  # Shift the arrow slightly outward
  x_label_adjust = 0.05,  # Adjust x-axis label position
  y_label_adjust = 4,  # Adjust y-axis label position
  title = "PC1 vs. PC2",
  x_label = "PC1",
  y_label = "PC2"
)


# Print the plot
print(test_plot)


library(Momocs)

# Example using a built-in dataset from Momocs
# We will use 'bot' dataset (botanical leaves) that comes with Momocs for demonstration

# Step 1: Load the dataset
bot_shapes <- bot %>% coo_center() %>% coo_scale() %>% coo_align()

# Step 2: Perform a Fourier transformation
bot_fourier <- bot_shapes %>% efourier(nb.h = 10)

# Step 3: Perform PCA on the Fourier coefficients
bot_pca <- PCA(bot_fourier)

# Now you have a valid `pca_result` and `coo_object` for use in your function
# bot_pca$x contains the PCA scores (similar to `prcomp` result)
# bot_pca and bot_shapes can now be passed to your function

# Step 4: Use the `shape_plot_momocs` function with proper inputs
shape_plot_momocs(bot_pca, bot_shapes,
                  pc_x = 2, pc_y = 3,  # PC axes for plotting
                  shape_size = 0.005, shift_distance = 0.1,
                  title = "PCA Shape Plot", x_label = "PC1", y_label = "PC2")
