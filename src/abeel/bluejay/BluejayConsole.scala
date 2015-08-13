package abeel.bluejay

object BluejayConsole {

  def main(args: Array[String]): Unit = {

    if (args.length == 0) {

      listInstructions
    } else {
      args(0) match {
        case "list" => listInstructions
        case "help" => listInstructions
        
        case "lineage-matrix" => MakeLineageMatrix.main(args.drop(1))
        case "variant-matrix" => MakeVariantMatrix.main(args.drop(1))
        case "associate" => RunBlueJay.main(args.drop(1))

        case _ => listInstructions
      }
    }

  }

  def listInstructions() {
    println("Usage:java -jar bluejay.jar [instruction] [instruction options...]")
    println("Instructions:")
   
    println("\tlineage-matrix         Build matrix from a lineage file")
    println("\tvariant-matrix         Build matrix from directory of VCF files")
    println("\tassociate              Associate lineages with variants")
    println("\tlist | help            Show help")
    

  }

}