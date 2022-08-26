import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:	"ClassicalMetaAnalysis"
		fromVersion:	"0.15"
		toVersion:		"0.16.4"

		ChangeRename { from: "dependent"; to: "effectSize" }
        ChangeRename { from: "wlsWeights"; to: "effectSizeStandardError" }
        ChangeRename { from: "includeConstant"; to: "includeIntercept" }
	}
}