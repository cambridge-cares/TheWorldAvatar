class PolyModel():
    # Set model coefficients on construction
    def __init__(self,order):
        if order < 0 or order > 2:
            raise ValueError("order must be between 0 and 2")
        all_coeffs = [2,-1,3]
        self.coeffs = all_coeffs[:order+1]
    
    # Evaluate the model at a particular value
    def evaluate(self,x):
        result = self.coeffs[0]
        for ii in range(1,len(self.coeffs)):
            result += self.coeffs[ii]*x**ii
        return result