# Python3 program to find the points
# which lies inside, outside or
# on the circle

# Function to find the line given
# two points
def lineFromPoints(P, Q, a, b, c):
	
	a = Q[1] - P[1]
	b = P[0] - Q[0]
	c = a * (P[0]) + b * (P[1])

	return a, b, c

# Function which converts the
# input line to its perpendicular
# bisector. It also inputs the
# points whose mid-lies o
# on the bisector
def perpenBisectorFromLine(P, Q, a, b, c):
	
	# Find the mid point
	mid_point = [0, 0]

	# x coordinates
	mid_point[0] = (P[0] + Q[0]) / 2

	# y coordinates
	mid_point[1] = (P[1] + Q[1]) / 2

	# c = -bx + ay
	c = (-b * (mid_point[0]) +
		a * (mid_point[1]))

	# Assign the coefficient of
	# a and b
	temp = a
	a = -b
	b = temp

	return a, b, c

# Returns the intersection of
# two lines
def LineInterX(a1, b1, c1, a2, b2, c2):
	
	# Find determinant
	determ = a1 * b2 - a2 * b1

	x = (b2 * c1 - b1 * c2)
	x /= determ

	return x

# Returns the intersection of
# two lines
def LineInterY(a1, b1, c1, a2, b2, c2):
	
	# Find determinant
	determ = a1 * b2 - a2 * b1

	y = (a1 * c2 - a2 * c1)
	
	#print(y)
	y /= determ

	return y

# Function to find the point
# lies inside, outside or on
# the circle
def findPosition(P, Q, R, D):
	
	# Store the coordinates
	# radius of circumcircle
	r = [0, 0]

	# Line PQ is represented
	# as ax + by = c
	a, b, c = lineFromPoints(P, Q, 0, 0, 0)

	# Line QR is represented
	# as ex + fy = g
	e, f, g = lineFromPoints(Q, R, 0, 0, 0)

	# Converting lines PQ and QR
	# to perpendicular bisectors.
	# After this, L = ax + by = c
	# M = ex + fy = g
	a, b, c = perpenBisectorFromLine(P, Q, 
									a, b, c)
	e, f, g = perpenBisectorFromLine(Q, R, 
									e, f, g)

	# The of intersection
	# of L and M gives r as the
	# circumcenter
	r[0] = LineInterX(a, b, c, e, f, g)
	r[1] = LineInterY(a, b, c, e, f, g)

	# Length of radius
	q = ((r[0] - P[0]) *
		(r[0] - P[0]) +
		(r[1] - P[1]) *
		(r[1] - P[1]))

	# Distance between radius
	# and the given D
	dis = ((r[0] - D[0]) *
		(r[0] - D[0]) +
		(r[1] - D[1]) *
		(r[1] - D[1]))

	# Condition for lies
	# inside circumcircle
	if (dis < q):
		print("Point (", D[0], ",", D[1],
			") is inside the circumcircle")

	# Condition for lies
	# on circumcircle
	elif (dis == q):
		print("Point (", D[0], ",", D[1], 
			") lies on the circumcircle")

	# Condition for lies
	# outside circumcircle
	else:
		print("Point (", D[0], ",", D[1], 
			") lies outside the circumcircle")

# Driver Code
if __name__ == '__main__':
	
	# A, B, C, D

	# Given Points
	A = [2, 2]
	B = [4, 1]
	C = [3, 4]
	D = [6, 3]

	# Function call to find
	# the lies inside,
	# outside or on the
	# circle
	findPosition(A, B, C, D)

# This code is contributed by mohit kumar 29
