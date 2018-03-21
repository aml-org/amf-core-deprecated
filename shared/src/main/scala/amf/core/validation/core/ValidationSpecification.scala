package amf.core.validation.core

import amf.core.vocabulary.Namespace
import org.yaml.model.YDocument.EntryBuilder

case class FunctionConstraint(message: Option[String],
                              code: Option[String] = None,
                              libraries: Seq[String] = Seq(),
                              functionName: Option[String] = None) {

  def constraintId(validationId: String)  = s"${validationId}Constraint"
  def validatorId(validationId: String)   = s"${validationId}Validator"
  def validatorPath(validationId: String) = s"${validationId}Path"
  def validatorArgument(validationId: String) = {
    "$" + validatorPath(validationId)
      .split("#")
      .last
      .replace("-", "_")
      .replace(".", "_")
  }
  def computeFunctionName(validationId: String) = functionName match {
    case Some(fnName) => fnName
    case _ => {
      val localName = validationId.split("/").last.split("#").last
      s"${localName.replace("-", "_").replace(".", "_")}FnName"
    }
  }
}

case class NodeConstraint(constraint: String, value: String)

case class PropertyConstraint(ramlPropertyId: String,
                              name: String,
                              // shacl:message
                              message: Option[String] = None,
                              pattern: Option[String] = None,
                              maxCount: Option[String] = None,
                              minCount: Option[String] = None,
                              minLength: Option[String] = None,
                              maxLength: Option[String] = None,
                              minExclusive: Option[String] = None,
                              maxExclusive: Option[String] = None,
                              minInclusive: Option[String] = None,
                              maxInclusive: Option[String] = None,
                              /**
                                * shacl:node
                                * Objects of this property must conform to the
                                * provided node shape
                                */
                              node: Option[String] = None,
                              datatype: Option[String] = None,
                              // format: Option[String] = None,
                              /**
                                * shacl:class
                                * Objects of this property must have this class
                                */
                              `class`: Seq[String] = Seq(),
                              in: Seq[String] = Seq.empty,
                              custom: Option[(EntryBuilder, String) => Unit] = None) {}

case class ValidationSpecification(name: String,
                                   // shacl:message
                                   message: String,
                                   ramlMessage: Option[String] = None,
                                   oasMessage: Option[String] = None,
                                   /**
                                     * shacl:targetNode
                                     * URIs of the nodes in the graph that will be
                                     * targeted by this shape
                                     */
                                   targetInstance: Seq[String] = Seq.empty,
                                   /**
                                     * shacl:targetClass
                                     * Nodes with these classes will be targeted
                                     * by this shape
                                     */
                                   targetClass: Seq[String] = Seq.empty,
                                   /**
                                     * shacl:targetObjectsOf
                                     *
                                     * Nodes that are object of the properties in
                                     * this array will be targeted by this shape
                                     */
                                   targetObject: Seq[String] = Seq.empty,
                                   /**
                                     * Union of constraints passed as URIs
                                     * to the contraints in the union
                                     */
                                   unionConstraints: Seq[String] = Seq.empty,
                                   /**
                                     * shacl:property
                                     * Property constraints for the node
                                     */
                                   propertyConstraints: Seq[PropertyConstraint] = Seq.empty,
                                   nodeConstraints: Seq[NodeConstraint] = Seq.empty,
                                   closed: Option[Boolean] = None,
                                   functionConstraint: Option[FunctionConstraint] = None,
                                   custom: Option[(EntryBuilder, String) => Unit] = None) {

  def id(): String = {
    if (name.startsWith("http://") || name.startsWith("https://") || name.startsWith("file:")) {
      name
    } else {
      Namespace.expand(name).iri() match {
        case s if s.startsWith("http://") || s.startsWith("https://") || s.startsWith("file:") => s
        case s                                                                                 => (Namespace.Data + s).iri()
      }
    }
  }

  def isParserSide() = targetInstance.nonEmpty && targetInstance.head == ValidationSpecification.PARSER_SIDE_VALIDATION
}

object ValidationSpecification {
  val PARSER_SIDE_VALIDATION: String = (Namespace.Shapes + "ParserShape").iri()
}
