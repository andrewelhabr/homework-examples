
# Use docplex.
from docplex.cp.model import CpoModel

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

# Define model.
wl = CpoModel()

# Define decision variables.
openwarehouse = wl.integer_var_list(nbWarehouses,0,1,"openwarehouse")
warehouseservesstore = wl.integer_var_list(nbStores,0,nbWarehouses-1,"warehouseservesstore")

# Define constraints.
for s in warehouseservesstore:
    wl.add(wl.element(openwarehouse,s) == 1)

for w in range(nbWarehouses):
    wl.add(wl.count(warehouseservesstore,w) <= capacity[w])

# Define objective.
fixedArray = []
for i in range(nbWarehouses):
    fixedArray.append(fixed)
total_cost = wl.scal_prod(openwarehouse,fixedArray)
for s in range(nbStores):
    total_cost = total_cost + wl.element(warehouseservesstore[s],supplyCost[s])

wl.add(wl.minimize(total_cost))

# Solve and print solution.
wlsol = wl.solve()
for w in range(nbWarehouses):
    if wlsol[openwarehouse[w]] == 1:
        print("Open warehouse " + str(w+1) + ".")
for s in range(nbStores):
    for w in range(nbWarehouses):
        if wlsol[warehouseservesstore[s]]==w:
          print("Store " + str(s+1) + " is serviced by warehouse " + str(w+1) + ".")
print("Total cost = $%.0f" % wlsol.get_objective_values()[0])
