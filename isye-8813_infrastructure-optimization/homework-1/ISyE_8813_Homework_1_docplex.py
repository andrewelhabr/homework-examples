
# Use docplex.
from docplex.mp.model import Model
from docplex.util.environment import get_environment

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

# Define variable ranges.
Stores = range(1,nbStores+1)
Warehouses = range(1,nbWarehouses+1)

# Define model.
wl = Model(name='Warehouse_Location')

# Define decision variables.
openwarehouse = {w: wl.binary_var(name='open_warehouse_{0}'.format(w)) for w in Warehouses}
route = {(s,w): wl.binary_var(name='store_{0}_from_warehouse_{1}'.format(s,w)) for s in Stores for w in Warehouses}

# Define objective.
wl.minimize(wl.sum(openwarehouse[w]*fixed for w in Warehouses) + wl.sum(route[s,w]*supplyCost[s-1][w-1] for s in Stores for w in Warehouses))

# Define constraints.
for s in Stores:
  wl.add_constraint(wl.sum(route[s,w] for w in Warehouses) == 1)

for w in Warehouses:
  wl.add_constraint(wl.sum(route[s,w] for s in Stores) <= capacity[w-1]*openwarehouse[w])

# Solve and print solution.
wl.solve()
wl.print_solution()
wl.report()
