trait PDE {
	def generateSolution(V: Boundary, xstep: Double, tstep: Double) : Array[Array[Double]]
}