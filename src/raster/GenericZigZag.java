package raster;

import java.awt.Point;
import java.util.Vector;

/*
 * Created on 11/04/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */

/**
 * @author Glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class GenericZigZag {

	protected int[][] matrix;
	private boolean mode, up;
	private int i, j;
	private int countNumbers;
	private Vector points;
	protected int width, height;

	/**
	 * Construtor para GenericZigZag
	 * @param matrix Matrix de entrada
	 * @param mode variavel que define o modo 
	 * (ascendente para true ou descendente para false).
	 */
	public GenericZigZag(int[][] matrix, boolean mode, int width, int height) {
		this.matrix = matrix;
		this.mode = mode;
		this.width = width;
		this.height = height;
	}

	/**
	 * Algoritmo que varre em zigue-zague. 
	 * @param mode True se ascendente. False se descendente.
	 * @param x Coordenadas iniciais em x.
	 * @param y Coordenadas iniciais em y.
	 * @param width Tamanho do bloco. 
	 * @param height  Tamanho do bloco.
	 * @param handler Interface
	 * @return Ponto específico da matriz
	 */
	protected Point algorithm(
		boolean mode,
		int x,
		int y,
		int width,
		int height,
		MatrixHandler handler) {
		if (mode)
			return processAsc(handler, x, y, width, height);
		return processDes(handler, x, y, width, height);
	}

	private Point nextDes(int i, int j, int limInferior, int limSuperior) {
		if (j + 1 == limSuperior || i == limInferior) {
			if (i != limInferior)
				i--;
			else
				j--;
			up = (up == true ? false : true);
		} else {
			i--;
			j++;
		}
		return new Point(i, j);
	}
	private Point nextAsc(int i, int j, int limInferior, int limSuperior) {
		if (i + 1 == limSuperior || j == limInferior) {
			if (i + 1 == limSuperior)
				j++;
			else
				i++;
			up = (up == true ? false : true);
		} else {
			i++;
			j--;
		}
		return new Point(i, j);
	}

	private Point processDes(
		MatrixHandler handler,
		int x,
		int y,
		int width,
		int height) {
		/*i = matrix.length-1;
		j = matrix[i].length-1;
		*/
		i = height - 1;
		j = width - 1;
		up = true;
		Point location = null;
		countNumbers = 0;
		Point p = null;
		while (j != y - 1) {
			if (handler != null) {
			location = handler.processDescendentHandle(matrix, i, j);
			if (location != null)
				return location;
			}
			if (up) {
				p = nextDes(i, j, x, width);
				i = p.x;
				j = p.y;
			} else {
				p = nextDes(j, i, y, height);
				j = p.x;
				i = p.y;
			}
			countNumbers++;
		}
		return null;
	}

	private Point processAsc(
		MatrixHandler handler,
		int x,
		int y,
		int width,
		int height) {
		i = x;
		j = y;
		up = false;
		Point location = null;
		countNumbers = 0;
		Point p = null;
		points = null;
		if (handler == null)
		 points = new Vector();
		while ((i != height || j != width) && countNumbers != (this.width * this.height)) {
			if (handler != null) {
			 location = handler.processAscendentHandle(matrix, i, j);
			 if (location != null)
				 return location;
			} else 
				points.add(new Point(i,j));
			if (!up) {
				p = nextAsc(i, j, y, height);
				i = p.x;
				j = p.y;
			} else {
				p = nextAsc(j, i, x, width);
				j = p.x;
				i = p.y;
			}
			countNumbers++;
		}
		return null;
	}
	/**
	 * @return
	 */
	public int getCountNumbers() {
		return countNumbers;
	}
	/**
	 * @return
	 */
	public Vector getPoints() {
		return points;
	}

}