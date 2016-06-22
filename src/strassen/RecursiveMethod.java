/*
 * Created on 28/01/2004
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
public class RecursiveMethod {

	private int n = 4;
	private int[][] a = {{1,2,3,6},
		                 {3,4,5,6},
		                 {2,3,3,8},
		                 {3,4,6,2}};
		                 
	private int[][] b = {{2,2,4,6},
		                 {3,3,8,2},
		                 {2,3,3,3},
		                 {6,4,6,4}};
	private int[][] c; 
	private int teste = 0;
	private int i,j;
	
	private MatrixStructure structure;
	
	
	public RecursiveMethod(int n) {
		this.n = n;
		c = new int[n][n];
	}
	
	//Logaritmo em base 2.
	 private double lg(double value) {
		 return (Math.log(value) / Math.log(2));
	 }
	
	private void generateStructure() {
		structure = new MatrixStructure(null);
		for(int i = 0; i < n; i++)
		 for(int j = 0; j < n; j++)
		  structure.fillStructure(i,j,a[i][j]);
	}
	
	public static void main(String[] args) {
		RecursiveMethod test = new RecursiveMethod(4);
		System.out.println(test);
	}
}
