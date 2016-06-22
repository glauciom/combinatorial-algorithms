/*
 * Created on 12/02/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package matrix;

/**
 * @author Glaucio Melo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class LUDecomposition {
	
	private int[][] matrix = {{2,3,1,5},
							  {6,13,5,19},
							  {2,19,10,23},
							  {4,10,11,31}};
	private int[][] L, U;
	private int n = 4;
	
	/**
	 * Constutor para LUDecomposition
	 * @param matrix Matriz contendo os valores
	 * @param n tamanho da matriz.
	 */
	public LUDecomposition(int n) {;
		this.n = 4;
		L = new int[n][n];
		U = new int[n][n];
		algorithm();
	}
	
	public void algorithm() {
		for(int k = 0; k < n; k++) {
		 U[k][k] = matrix[k][k];
		 for(int i = k; k < n-1; i++) {
		  L[i][k] = matrix[i][k] / U[k][k];
		  U[k][i] = matrix[k][i];
		 }
		 for(int i = k; i < n; i++)
		  for(int j = k; j < n; j++)
		   matrix[i][j] = matrix[i][j] - L[i][k] * U[k][j];  
		}
		int[][] l = L;
		int[][] u = U;
		return;	 
	}
	
	
	
	public static void main(String[] args) {
	 new LUDecomposition(4);
	}
}
