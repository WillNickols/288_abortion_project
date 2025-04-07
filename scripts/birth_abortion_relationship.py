import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
import numpy as np

# Set the style for the plots
plt.style.use('seaborn')
sns.set_palette(['blue', 'darkgreen', 'red'])

# Load the data
abortion_diffs = pd.read_csv('data/abortion_diffs.csv')
birth_diffs = pd.read_csv('data/birth_diffs.csv')

# Calculate actual counts from rates
abortion_diffs['AbortionCount'] = abortion_diffs['AbortionsPerThousandWomen'] * abortion_diffs['Population'] / 1000
birth_diffs['BirthCount'] = birth_diffs['BirthsPerThousandWomen'] * birth_diffs['Population'] / 1000

# Merge the data
merged_data = pd.merge(
    abortion_diffs[['State', 'Year', 'Month', 'AbortionCount', 'TreatedOriginal']],
    birth_diffs[['State', 'Year', 'Month', 'BirthCount']],
    on=['State', 'Year', 'Month'],
    how='outer'
)

# Create the plot
plt.figure(figsize=(10, 8))
sns.scatterplot(
    data=merged_data,
    x='AbortionCount',
    y='BirthCount',
    hue='TreatedOriginal',
    alpha=0.6,
    s=100
)

# Add regression lines for each group
for group in merged_data['TreatedOriginal'].unique():
    group_data = merged_data[merged_data['TreatedOriginal'] == group]
    if len(group_data) > 1:  # Only plot regression if we have enough points
        model = LinearRegression()
        X = group_data[['AbortionCount']]
        y = group_data['BirthCount']
        model.fit(X, y)
        x_range = np.linspace(X.min(), X.max(), 100)
        y_pred = model.predict(x_range.reshape(-1, 1))
        plt.plot(x_range, y_pred, label=f'{group} trend', alpha=0.5)

plt.title('Relationship Between Monthly Abortion and Birth Counts\nBy State Type (2022-2023)')
plt.xlabel('Monthly Abortion Count')
plt.ylabel('Monthly Birth Count')
plt.legend(title='State Type')

# Save the plot
plt.tight_layout()
plt.savefig('figures/birth_abortion_relationship.png', dpi=300, bbox_inches='tight')

# Print summary statistics
print("\nSummary Statistics:")
print(f"Correlation between abortion and birth counts: {merged_data['AbortionCount'].corr(merged_data['BirthCount']):.3f}")

# Print regression results
print("\nRegression Results:")
# Create dummy variables for TreatedOriginal
dummies = pd.get_dummies(merged_data['TreatedOriginal'], drop_first=True)
X = pd.concat([merged_data[['AbortionCount']], dummies], axis=1)
y = merged_data['BirthCount']

model = LinearRegression()
model.fit(X, y)

# Print coefficients
print("\nCoefficients:")
for feature, coef in zip(X.columns, model.coef_):
    print(f"{feature}: {coef:.3f}")
print(f"Intercept: {model.intercept_:.3f}")

# Print R-squared
print(f"\nR-squared: {model.score(X, y):.3f}") 