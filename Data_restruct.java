import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.PrintStream;
import java.util.List;
import java.util.Scanner;

import au.com.bytecode.opencsv.CSVReader;
import au.com.bytecode.opencsv.CSVWriter;


public class Data_restruct {

	public static void main(String[] args) throws FileNotFoundException {
		
		// get location to read from
		String FilePath = "MovieTweetings" + File.separator + "users.csv";
		String FilePath_3 = "MovieTweetings" + File.separator + "movies.csv";
		String FilePath_5 = "MovieTweetings" + File.separator + "ratings.csv";
		String FilePath_8 = "MovieLens" + File.separator + "u.csv";
		String FilePath_11 = "MovieLens" + File.separator + "umovie.csv";
		CSVReader reader = null;
		// create a new file to write to
		String FilePath_2 = "users_mod.csv";
		String FilePath_4 = "movies_mod.csv";
		String FilePath_6 = "temp.csv";
		String FilePath_7 = "ratings_mod.csv";
		String FilePath_9 = "u_mod.csv";
		String FilePath_10 = "movieLens_movie_mod.csv";
		CSVWriter writer = null;
		
		// set up scanner object to read the movie.csv file
		File File = new File(FilePath_3);
		Scanner scan = new Scanner(File);
		
		// set delimiters to use 
		scan.useDelimiter("::|\\|");
		
		try {
			/*
			 * reading in and writing out user.csv -- twitter
			 */
			
			// initiate reader and set delimiter to :
			reader = new CSVReader(new FileReader(FilePath), ':', '"');
			// initiate writer
			writer = new CSVWriter(new FileWriter(FilePath_2));
			
			// create a list and read whole file with new separated columns
			List<String[]> allRows = reader.readAll();
			// write new data to a new file -- FilePath_2
			writer.writeAll(allRows);
			
			// close reader and writer
			reader.close();writer.close();
			
			/*
			 * Read from movie tweetings -- movies.csv to a new file with better structure
			 */
			
			// change stdout to the specified file instead of command output
			PrintStream out = new PrintStream(new FileOutputStream("movies_mod.csv"));
			// Save the command output to revert back to later
			PrintStream old = System.out;
			// Tells Java to print all output to file
			System.setOut(out);
		
			// iterate through scanned file
			while(scan.hasNext()) {
				String s = scan.next();
				// prints to file instead of command output
				System.out.print(s + ",");
				
			}
			//close the scanner and the file once finished
			scan.close(); out.close();
			// reset the stdout to command output
			System.out.flush(); System.setOut(old);
			
			/*
			 * modify the ratings.csv file and save to new file -- twitter
			 */
			
			// initiate reader and set delimiter to :
			reader = new CSVReader(new FileReader(FilePath_5), ':', '"');
			// initiate writer
			writer = new CSVWriter(new FileWriter(FilePath_6));
			
			// create a list and read whole file with new separated columns
			List<String[]> allRatings = reader.readAll();
			// write new data to a new file
			writer.writeAll(allRatings);
			
			// close reader and writer
			reader.close();writer.close();
			
			// read modified file and restructure to delete empty cells and write to new
			reader = new CSVReader(new FileReader(FilePath_6));
			writer = new CSVWriter(new FileWriter(FilePath_7));
			// initiate string object to tokenise
			String[] line;
			
			// iterate line by line through file and re-structure tabs
			while ((line = reader.readNext()) != null) {
				line[0] = line[0];
				line[1] = line[2];
				line[2] = line[4];
				line[3] = line[6];
				writer.writeNext(line[0],line[1],line[2],line[3]);
			}
			
			// close reader and writer
			reader.close();writer.close();
			
			/*
			 * read in and restructure u.csv -- movielens
			 */
			
			reader = new CSVReader(new FileReader(FilePath_8), '\t');
			writer = new CSVWriter(new FileWriter(FilePath_9));
			
			List<String[]> movieLens_u = reader.readAll();
			writer.writeAll(movieLens_u);
			
			reader.close();writer.close();
			
			/*
			 * restructure
			 */
			
			/*
			 * read from movielens -- umovie.csv to new file for better structure
			 */
			
			// initiate reader and writer
			reader = new CSVReader(new FileReader(FilePath_11), '|');
			writer = new CSVWriter(new FileWriter(FilePath_10));
			
			// create list object which holds each line of file in correct structure
			List<String[]> lens_movies = reader.readAll();
			// write list object to new file
			writer.writeAll(lens_movies);
			// close the reader and writer
			reader.close(); writer.close();

			} catch(Exception e){
				e.printStackTrace();
		}

	}

}
