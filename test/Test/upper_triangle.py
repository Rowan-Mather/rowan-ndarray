import numpy as np

x = np.array([2,5,1, 9,2,7, 4,16,3], dtype=float).reshape(3,3)
print(x)

#first index is y 
print(x[1,2])
print()
for i in range(3):
    for j in range(i+1,3):
        ratio = x[j,i] / x[i,i]
        for k in range(3):
            x[j,k] = x[j,k] - ratio * x[i,k]
            print(i,j,k, ratio)
            

print()
print(x)
print(np.linalg.det(x))
