/*
 * Created on 21/01/2004
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
public class StrassenMethod {
	
	// Dimensão da matriz;
	private int n = 9;
	
	private int[] s = new int[4];
	private int[] t = new int[4];
	private int[][] a = {{1,2},{3,4}};
	private int[][] b = {{2,2},{3,3}};
	private int[][] c = new int[2][2];
	private int[] p = new int[7];
	private int[] u = new int[7];
	
	private void init() {
		s[0] = a[1][0] + a[1][1];
		s[1] = s[0] - a[0][0];
		s[2] = a[0][0] - a[1][0];
		s[3] = a[0][1] - s[1];
		
		t[0] = b[0][1] - b[0][0];
		t[1] = b[1][1] - t[0];
		t[2] = b[1][1] - b[0][1];
		t[3] = b[1][0] - t[1];
		
		p[0] = a[0][0] * b[0][0];
		p[1] = a[0][1] * b[1][0];
		p[2] = s[0]  * t[0];
		p[3] = s[1] * t[1];
		p[4] = s[2] * t[2];
		p[5] = s[3] * b[1][1];
		p[6] = a[1][1] * t[3];
		
		u[0] = p[0] + p[1];
		u[1] = p[0] + p[3];
		u[2] = u[1] + p[4];
		u[3] = u[2] + p[6]; 
		u[4] = u[2] + p[2];
		u[5] = u[1] + p[2];
		u[6] = u[5] + p[5];
		
		c[0][0] = u[0];
		c[0][1] = u[6];
		c[1][0] = u[3];
		c[1][1] = u[4];
		
	}
	// Logaritmo em base 2.
	private double lg(double value) {
		return (Math.log(value) / Math.log(2));
	}
	
	// Método núcleo que valida a entrada.
	private boolean isValidDimension(double d) {
		int aux = (int) d;
		double frac = d - aux;
		if (frac == 0)
		 return true;
	return false;
	}
	
	/**
	 * Aceita a entrada da matriz como sendo uma potência de 2.
	 * @param d entrada da dimensão da matriz
	 * @return true caso aceite a entrada; false, do contrário.
	 */
	public boolean acceptEntry(double d) {
		return isValidDimension(d);
	}

	/**
	 * Construtor para StrassenMethod.
	 * @param n Dimensão da matriz.
	 */
	public StrassenMethod(int n) {
		this.n = n;
		init();
	}
	
	public String toString() {
		String s = "";
		for(int i = 0; i < 2; i++) {
		 for(int j = 0; j < 2; j++)
		  s += c[i][j] + " ";
		 s += "\n";
		}
	return s;
	}
	
	
	
	
	/**
	 * Método main (para testes).
	 * @param args String de entrada (para parâmetros de entrada em prompt).
	 */
	public static void main(String[] args) {
		
		StrassenMethod test = new StrassenMethod(2);
		System.out.println(test);
		
	}
}
