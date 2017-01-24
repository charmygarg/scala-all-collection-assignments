case class StudentDetail(studentid:Long,studentName:String)
case class Markss(subjectid:Long,studentid:Long,marks:Float)
case class ScoreCard(studentId: Long, marks: Map[Long, Float], percentage: Float)

object MapStudent extends App {
  
  val studentlist=List(StudentDetail(1,"shubham"),StudentDetail(2,"charmy"),StudentDetail(3,"osho"),StudentDetail(4,"shubham"))
  val markslist=List( Markss(1,1,100), Markss(1,2,100),Markss(1,3,100),Markss(1,4,100),
                      Markss(2,1,40), Markss(2,2,100), Markss(2,3,90), Markss(2,4,100),
                      Markss(3,1,50), Markss(3,2,30),  Markss(3,3,80), Markss(3,4,100) ,
                      Markss(4,1,70), Markss(4,2,80) , Markss(4,3,20), Markss(4,4,100) )
                     
  val y  =   markslist.groupBy(_.studentid)
                  
  val output=y.map(x=>(x._1,x._2.map(y=> (y.subjectid,y.marks)).toMap,x._2.map(y=> y.marks).sum/4))
                     
  val scorecardlist = output.map(x=> ScoreCard(x._1,x._2,x._3) )
   
  val mapper = buildMap() 
              
  println(mapper)
                            
  studentsReportCardByName("shubham")            // calling second method by passing student name
        
              
            
    /* this function will return map which will have the key student name and 
              value will have the reportcard */
         
 def  buildMap(): Map[String,AnyRef] =
  {
         val newmap=  for{i<-scorecardlist;j<-studentlist if(i.studentId==j.studentid)}yield (j.studentName,i)
         val temp=newmap.groupBy(x=>x._1)
         temp.map(x=> (x._1,x._2.map(y=>y._2)))
        
   }
              
   // this function will take student name and will print the ScoreCard        
              
 def studentsReportCardByName(studentName:String)=
   {
      if(mapper.keys.toSeq.contains(studentName))
      {
           val recordcard= for{ i<-mapper if(i._1==(studentName))
           val record=(i._2)  } yield record
           
           recordcard.map(println _)
      } 
      else println("Name not found") 
    
    }
}
