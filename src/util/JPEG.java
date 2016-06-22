/*
 * Created on 12/04/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package util;

import java.awt.Point;
import java.io.File;
import java.util.Vector;

import raster.DCTZigZag;

/**
 * @author euneto
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class JPEG {
	
	private int width, height, levels;
	
	private int originalMatrix[][]; //Para o cálculo posterior do MSE
	private long tamanhoOriginal=0; //Para o cálculo da taxa de compressão
	private long tamanhoComprimido=0; //Para o cálculo da taxa de compressão
	
	private int matrix[][];
	private double[][] q = {{16,11,10,16,24,40,51,61},
							{12,12,14,19,26,58,60,55},
							{14,13,16,24,40,57,69,56},
							{14,17,22,29,51,87,80,62},
							{18,22,37,56,68,109,103,77},
							{24,35,55,64,81,104,113,92},
							{49,64,78,87,103,121,120,101},
							{72,92,95,98,112,100,103,99}};
	
	private int[][] zigzagMatrix;
	
	public JPEG(File f, int width, int height) {
		this.tamanhoOriginal = f.length();
		this.matrix = ReadingWritingInput.readMatrix(f);
		this.levels = ReadingWritingInput.levels + 1;
		this.levels = (int)(Math.log(levels) / Math.log(2)) - 1;
		
	}
	
	public void guardarMatrixOriginal(){
		this.originalMatrix = new int[this.matrix.length][this.matrix[0].length];
		for (int i=0; i<this.matrix.length; i++){
			for (int j=0; j<this.matrix[i].length; j++){
				this.originalMatrix[i][j] = this.matrix[i][j];
			}
		}
	}
	
	
	public void run(File fileOutput, int width, int height) {
			guardarMatrixOriginal();
			int[] d = null; 			//Vetor contendo o resultado da linha corrente do ZigZag
			int a = width; 				//Variavel auxiliar
			Vector v = new Vector(); 	//Estrutura que irah armazenar o vetor irregular (saida do ZigZag)
			this.width = width; 		//Altura da matriz
			this.height = height; 		//Largura da matriz
			DCTZigZag zigzag = new DCTZigZag(matrix, this.width, this.height);
			
			for (int i = 0; i < matrix.length; i = i + this.width) {
				for (int j = 0; j < matrix[i].length; j = j + this.height) {
					rasterBlock(new Point(i,j), new Point(a, j + height));
					d = zigzag.algorithm(i, j, j + height, a);
					v.addElement(d);
				}
				a += width;
			}
			zigzagMatrix = zigzag.copyArrays(v);
			ReadingWritingInput.writeZigZag(zigzagMatrix,width,height,fileOutput);
			this.tamanhoComprimido=fileOutput.length();
	}

	public void comeback(File fileInput, File fileOutput, int width, int height) {
		int[] d = null; 			//Vetor contendo o resultado da linha corrente do ZigZag
		int a = width; 				//Variavel auxiliar
		Vector v = new Vector(); 	//Estrutura que irah armazenar o vetor irregular (saida do ZigZag)
		this.width = width; 		//Altura da matriz
		this.height = height; 		//Largura da matriz
		int[][] input = ReadingWritingInput.readZigZag(fileInput);
		DCTZigZag zigzag = new DCTZigZag(matrix, this.width, this.height);
		zigzag.copyValues(this.matrix,input,this.width,this.height);
		for (int i = 0; i < matrix.length; i = i + this.width) {
			for (int j = 0; j < matrix[i].length; j = j + this.height) {
				unRasterBlock(new Point(i,j), new Point(a, j + height));
			}
			a += width;
		}
		ReadingWritingInput.writeMatrix(this.matrix,fileOutput); //Escreve a matriz   
	}
	
	private void unRasterBlock(Point p1, Point p2) {
//		Recuperar bloco da para a matriz
		int [][] y = RecuperaBlocoMatriz(p1);

//		desquantização
		desquantizar(y);
		
//		DCT
		int[][] x = new int[height][width];
		
		for (int l = 0; l < y.length; l++) {
			for (int c = 0; c < y[l].length; c++) {
				x[l][c] = (int)Math.round(dctInversa(y, l, c));
			}
		}
		
		jogaBlocoMatriz(p1, x);
		
		//Normalização
		desnormalizar(p1,p2);

		
		
			   
	}

	private void rasterBlock(Point p1, Point p2) {
		//Normalização
		normalizar(p1,p2);
		
		//DCT
		int[][] x = preparaDCT(p1);
		int[][] y = new int[height][width];
		for (int l = 0; l < y.length; l++) {
			for (int c = 0; c < y[l].length; c++) {
				y[l][c] = (int)Math.round(dct(x, l, c));
				//System.out.print(y[l][c] +  " ");
			}
			//System.out.println();
		}
		
		//Quantização
		quantizar(y);
		
		//Enviar bloco de volta para a matriz
		jogaBlocoMatriz(p1, y);
	}
	
	private void jogaBlocoMatriz(Point p1, int[][] bloco) {
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < width; j++) {
				matrix[i + p1.x][j + p1.y] = bloco[i][j];
			}
		} 
	}
	
	private int[][] RecuperaBlocoMatriz(Point p1) {
		int[][] bloco = new int[height][width];
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < width; j++) {
				bloco[i][j] = matrix[i + p1.x][j + p1.y];
			}
		} 
		return bloco;
	}
	
	private void normalizar(Point p1, Point p2) {
		for (int i = p1.x; i < p2.x; i++) {
			for (int j = p1.y; j < p2.y; j++) {
				matrix[i][j] -= (int)Math.pow(2,this.levels);
			}
		}  	
	}
	
	private void desnormalizar(Point p1, Point p2) {
		for (int i = p1.x; i < p2.x; i++) {
			for (int j = p1.y; j < p2.y; j++) {
				
				matrix[i][j] += (int)Math.pow(2,this.levels);
				
				matrix[i][j] = matrix[i][j] > 255	? 255	:matrix[i][j];
				matrix[i][j] = matrix[i][j] < 0		? 0		:matrix[i][j];
			}
		}  	
	}
	
	private int[][] preparaDCT(Point p1) {
		int mat[][] = new int[height][width];
		for (int i = 0; i < height; i++) {
			for (int j = 0; j < width; j++) {
				mat[i][j] = matrix[i + p1.x][j + p1.y];
			}
		} 
		return mat;
	}
	
	private double dct(int bloco[][], int u, int v) {
		double acumulado = 0;
		double cu = 1;
		double cv = 1;
		for (int x = 0; x < bloco.length; x++) {
			for (int y = 0; y < bloco[x].length; y++) {
				acumulado += bloco[x][y] * Math.cos(((2 * x + 1) * u * Math.PI) / 8.0) * Math.cos(((2 * y + 1) * v * Math.PI) / 8.0);
			}
		}
		if (u == 0) {
			cu = 1 / Math.sqrt(2);
		}
		if (v == 0) {
			cv = 1 / Math.sqrt(2);
		}
		return acumulado * 1/16 * cu * cv;   
	}
	
	private double dctInversa(int bloco[][], int x, int y) {
		double acumulado = 0;
		double cu;
		double cv;
		
		for (int u = 0; u < bloco.length; u++) {
			for (int v = 0; v < bloco[u].length; v++) {
				cu =  u==0 ? (1 / Math.sqrt(2)):1;
				cv =  v==0 ? (1 / Math.sqrt(2)):1;
				acumulado += cv * cu * bloco[u][v] * Math.cos(((2 * x + 1) * u * Math.PI) / 8.0) * Math.cos(((2 * y + 1) * v * Math.PI) / 8.0);
			}
		}
		
		return acumulado/16.0;   
	}
	
	private void quantizar(int[][] bloco) {
		double saida = 0;
		for (int x = 0; x < bloco.length; x++) {
			for (int y = 0; y < bloco[x].length; y++) {
				bloco[x][y] = (int)Math.round(bloco[x][y] / q[x][y]);
			}
		}
	}
	
	private void desquantizar(int[][] bloco) {
		double saida = 0;
		for (int x = 0; x < bloco.length; x++) {
			for (int y = 0; y < bloco[x].length; y++) {
				bloco[x][y] = (int)Math.round(bloco[x][y] * q[x][y]);
			}
		}
	}
	
	public String toString() {
		String saida = "";
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[i].length; j++) {
				saida += matrix[i][j] + " ";
			}
			saida += "\n";
		}
		return saida;
	}
	
	public double getMSE(){
		double mse=0;
		for (int i = 0; i < this.originalMatrix.length; i++) {
			for (int j = 0; j < this.originalMatrix[i].length; j++) {
				mse += Math.pow((this.originalMatrix[i][j] - this.matrix[i][j]),2); //Somatorio do erro medio quadratico
			}
		}
		mse /= (this.originalMatrix.length * this.originalMatrix[0].length); //Erro medio quadratico calculado
		return mse;
	}
	
	public double getTaxaCompressao(){
		return ((double)this.tamanhoOriginal/(double)tamanhoComprimido);
	}
	
	/**
	 * @return
	 */
	public long getTamanhoComprimido() {
		return tamanhoComprimido;
	}

	/**
	 * @return
	 */
	public long getTamanhoOriginal() {
		return tamanhoOriginal;
	}

	public String getInfo(){
		return "MSE:"+getMSE()+"\nTaxa:"+getTaxaCompressao()+"\nTamanhoOriginal:"+getTamanhoOriginal()+" bytes\nTamanhoComprimido:"+getTamanhoComprimido()+" bytes";
	}

	public static void main(String[] args) {
		int width = 4; int height = 4;
		JPEG jpeg = new JPEG(new File("./lena.pgm"),width,height);
		jpeg.run(new File("./outputMatrix.pgm"),width,height);
		jpeg.comeback(new File("./outputMatrix.pgm"), new File("./reconstrucao.pgm"),width,height);
		System.out.println(jpeg.getMSE());
		System.out.println(jpeg.getInfo());
	}


}
