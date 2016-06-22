/*
 * Created on 28/01/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package strassen;

import java.awt.Point;

/**
 * @author Glaucio Melo
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class MatrixElement {
	
	private double element;
	private Point coordinate;
	private MatrixStructure structure;
	
	
	public MatrixElement(Point coordinate, double element) {
		this.coordinate = coordinate;
		this.element = element;
		this.structure = null;
	}
	
	
	public MatrixElement(int i, int j, double element) {
		this(new Point(i,j),element);
	}
	
	
	
	
	
	/**
	 * @return
	 */
	public double getElement() {
		return element;
	}

	/**
	 * @param d
	 */
	public void setElement(double d) {
		element = d;
	}

	/**
	 * @return
	 */
	public Point getCoordinate() {
		return coordinate;
	}


	/**
	 * @param point
	 */
	public void setCoordinate(Point point) {
		coordinate = point;
	}
	/**
	 * @return
	 */
	public MatrixStructure getStructure() {
		return structure;
	}

	/**
	 * @param structure
	 */
	public void setStructure(MatrixStructure structure) {
		this.structure = structure;
	}

}
