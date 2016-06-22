/*
 * Created on 11/04/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package raster;

import java.awt.Point;

/**
 * @author Glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public interface MatrixHandler {

	public Point processAscendentHandle(int[][] d, int i, int j);

	public Point processDescendentHandle(int[][] d, int i, int j);
}
