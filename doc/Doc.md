# CLI Mandelbrot Tool

This project aims to provide a tool for plotting the mandelbrot set in the command line. 
It is configurable, with the following parameters and defaults:

Height and Width of plot:
The provided Height (pH) and Width (pW) of the plot are Natural Numbers, excluding 0.
Assuming default zoom, the subsection of the complex plane plotted is the square with x-values between (-2+0i,2+0i)
and y-values between (0-2i,0+2i). We will call this the Standard Section. Each unit of the pH and pW values determines the number of cursor-sized pixels 
used to plot the image. This is done by simply dividing the height and width of the Standard Section by the pH pW values provided. 
The Complex value of each pixel is the centre value of where the pixel is on the Standard Section. Since the cursor is approximately twice as tall as it is wide,
the plot only has the correct proportions when pW = 2*pH.
	Default : 16 32.
	These are pH and pW in that order.
	
Palette: 
Our palette are the colours that will be used to plot the set. The palette is defined by 1 Colour and a list of Colours. The one colour is the colour you
want to print the values that are in the set. The list is the colours that you want used for values outside of the set, since values not part of the set are
partioned by the speed at which they diverge under the function, each colour in the list will be used for a partion, with the list ordered by from fastest to diverge 
to slowest.
	Default: (255 179 179, 255 102 102, 255 25 25, 204 0 0, 153 0 0, 122 0 0, 92 0 0), 0 0 0
	Darker shades of red slower the divergence, then black inside the set.
	Input format: Please do not use any other bracketing other than (). Please ensure each value is >= 0 and <= 255. Please pass the brackted list in first and the single value second, please seperate each value by only ONE space. Please seperate the list and the single value with a comma	
   

Zoom:
The Zoom is a pair of complex values by which we will add and then multiply each value of the Standard Section to achieve our new subsection of the complex plane. This will move the centre of the 
of the complex number and rotate by the Argument of the complex number.
	Default: 0 + 0i, 1 + 0i
    Input format: Make sure to seperate the two values with a comma
		          Please ensure you use + in the centre and i on the second value for the complex numbers. If you want to use negative values please wrap it in brackets.
	

ComputationInfo:
This is information specifying the nature of the computation to perform. The components of this are: func, orbitRad (positive double), initVal (complex number), maxIterations (positive integer). The func is the complex function to evaluate to form the fractal, how exactly this occurs is based on the grid that it is being applied to and the orbitRad, initVal, maxIterations values. The func takes two values z and c. c is a constant that takes the value of the grid coordinate. For the initial iteration z takes the value provided in initVal. After func has been applied the return complex number is passed to the next iteration as the z value. This continues for maxIterations times. If the magnitude of the value returned from any of these iterations ever is strictly greater than orbitRad then we take the a value from the Palette list propotional to number of iterations taken, otherwise we assign it the "inside-set" colour.
	Default: z*z + c, 2, 0+0i, 1000
	Input format: Please seperate each value with a comma. In the function valid operations include + - * ^ /. In the function please do not use any charecters barring z c and i. For decimal point seperator use '.'
