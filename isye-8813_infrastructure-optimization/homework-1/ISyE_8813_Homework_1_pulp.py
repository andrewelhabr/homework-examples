
# Import pulp.
from pulp import *

# Define given data.
fixed = 30
nbStores = 10
nbWarehouses = 5
capacity = [1,4,2,1,3]
supplyCost = [
   [20,24,11,25,30],
   [28,27,82,83,74],
   [74,97,71,96,70],
   [2,55,73,69,61],
   [46,96,59,83,4],
   [42,22,29,67,59],
   [1,5,73,59,56],
   [10,73,13,43,96],
   [93,35,63,85,46],
   [47,65,55,71,95]
]

# Define lists for variable creation.
Stores = [1+i for i in range(nbStores)]
Warehouses = [1+i for i in range(nbWarehouses)]
Routes = [(s,w) for s in Stores for w in Warehouses]

# Label the problem.
prob = LpProblem('Warehouse Location',LpMinimize) 

# Define variables.
openwarehouseVars = LpVariable.dicts("Open Warehouse",Warehouses,0,1,LpBinary)
routeVars = LpVariable.dicts("Route",(Stores,Warehouses),0,1,LpBinary)

# Define objective function.
prob += lpSum([openwarehouseVars[w]*fixed  for w in Warehouses] + [routeVars[s][w]*supplyCost[s-1][w-1] for (s,w) in Routes]), "Sum of Fixed and Transportation Costs"

# Define constraints.
for s in Stores:
    prob += lpSum([routeVars[s][w] for w in Warehouses]) == 1, "Store " + str(s) + " must be served"

for w in Warehouses:
    prob += lpSum([routeVars[s][w] for s in Stores]) <= capacity[w-1]*openwarehouseVars[w], "Capacity for warehouse " + str(w) + " cannot be exceeded"

# Solve problem with CPLEX solver.
prob.solve(CPLEX())

# Print output.
print()
print("---------The variables selected in the optimal solution to the warehouse location problem are----------")
#print("Status:", LpStatus[prob.status])
for v in prob.variables():
    if v.varValue == 1.0:
        print(v.name)
print()
print("Total fixed and transportation costs = $%.2f" % value(prob.objective))
print("-------------------------------------------------------------------------------------------------------")
print()
