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
public class MatrixStructure {

	private MatrixElement[][] elements;
	
	public MatrixStructure(MatrixElement[][] elements) {
		this.elements = new MatrixElement[2][2];
	}
	
	public static void main(String[] args) {
	}
	/**
	 * @return
	 */
	public MatrixElement[][] getElements() {
		return elements;
	}

	/**
	 * @param elements
	 */
	public void setElements(MatrixElement[][] elements) {
		this.elements = elements;
	}
	
	public MatrixElement[][] fillStructure(int i, int j, double value) {
	 for(int w = 0; w < 2; w++)
	  for(int z = 0; z < 2; z++)
	   elements[i][j] = new MatrixElement(i,j,value);	   
	 return elements;
	}

}
