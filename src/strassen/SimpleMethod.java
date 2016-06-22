/*
 * Created on 23/01/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package strassen;

/**
 * @author Glaucio Melo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class SimpleMethod {
	
	private int n = 4;
	private int[][] a = {{1,2,3,6},{3,4,5,6},{2,3,3,8},{3,4,6,2}};
	private int[][] b = {{2,2,4,6},{3,3,8,2},{2,3,3,3},{6,4,6,4}};
	/*private int[][] a = {{1,2,3,6,5,5,5,5},
						 {3,4,5,6,5,5,5,5},
						 {2,3,3,8,5,5,5,5},
						 {3,4,6,2,5,5,5,5},
						 {1,2,3,6,5,5,5,5},
						 {3,4,5,6,5,5,5,5},
						 {2,3,3,8,5,5,5,5},
						 {3,4,6,2,5,5,5,5}};
	                 
	private int[][] b = {{1,2,3,6,5,5,5,5},
						 {3,4,5,6,5,5,5,5},
						 {2,3,3,8,5,5,5,5},
						 {3,4,6,2,5,5,5,5},
						 {1,2,3,6,5,5,5,5},
						 {3,4,5,6,5,5,5,5},
						 {2,3,3,8,5,5,5,5},
						 {3,4,6,2,5,5,5,5}};*/
	private int[][] c;


	public SimpleMethod(int n) {
		this.n = n;		
		c = new int[n][n];
		c = multiply(a,b);
	}
	
	public int[][] multiply(int[][]a, int[][] b) {
	 for(int i = 0; i < n; i++)
	  for(int j = 0; j < n; j++)
	   for(int k = 0; k < n; k++)
	    c[i][j] += a[i][k] * b[k][j];
	
	return c;
		
	}
	
	
	public String toString() {
		String s = "";
		for(int i = 0; i < c[0].length; i++) {
		 for(int j = 0; j < c.length; j++)
		  s += c[i][j] + " ";
		 s += "\n";
		}
	return s;
	}	
	
	public static void main(String[] args) {
		SimpleMethod test = new SimpleMethod(4);
		System.out.println(test);
	}
}
