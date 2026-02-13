---
applyTo: "**/inst/qml/*.qml"
---

# JASP QML Instructions

## 1) Core Basics

- **Imports:**
  ```qml
  import QtQuick
  import QtQuick.Layouts
  import JASP.Controls
  import JASP
  ```

- **Form as root:** Every analysis UI is a `Form { ... }` containing controls, usually a `VariablesForm` block and option controls.
- **Binding & IDs:** Prefer *property bindings* (reactive JS expressions) over imperative changes; reference other items via `id:` and bind (`enabled: show.checked || useAlt.checked`).
- **Stable storage names:** The `name:` of a control maps to stored options in JASP files; **avoid renaming**. If you must, handle migrations in `Upgrades.qml`.
- **Translation & docs:**
  - Wrap **all user-visible strings** in `qsTr("Text")`.
  - Populate `info:` with a short, user-facing description (also wrapped in `qsTr`) to feed module help.
- **Variables workflow:** Place variable pickers inside a `VariablesForm`; connect lists with `source:` (can read all data columns, other lists, levels, or R sources).

## 2) Input Validation

Prefer **declarative validation** via built-in field properties:

- **Numeric fields** (`DoubleField`, `IntegerField`): set `min`, `max`, and `inclusive` (e.g., `MinMax`), `decimals` (for doubles), and allow negatives only when needed. Use `fieldWidth` for compact UI.
- **Percent & CI** (`PercentField`, `CIField`): sensible defaults (e.g., 95), `afterLabel` defaults to `"%"`.
- **Slider:** set `min`, `max`, `decimals`; prefer horizontal sliders unless space constrained.
- **FormulaField:** accepts R-style expressions; constrain with `min`, `max`, `inclusive`; use `multiple: true` only when arrays are intended. Read via `realValue` / `realValues`.
- **TableView:** for mixed types, define validators and override `getValidator(col,row)`; optionally specify `itemTypePerRow/Column`.
- **Variables lists:** enforce data types via `allowedColumns: ["scale"|"ordinal"|"nominal"]` and `singleVariable: true` where appropriate.

## 3) Main Custom Components

### General input
- **CheckBox** — `name`, `label`, `checked`, `childrenOnSameRow`, `columns` (nested controls auto-enable/disable).
- **RadioButtonGroup / RadioButton** — group has `name`, `title`, `radioButtonsOnSameRow`, `columns`; each button has `value`, `label`, `checked`; can contain nested controls per choice.
- **DropDown** — `name`, `label`, `values` (array or `{label, value}`), or `source`; selection via `startValue` / `currentValue`; `addEmptyValue`, `placeHolderText`.
- **Slider** — `name`, `label`, `value`, `min`, `max`, `decimals`.
- **DoubleField / IntegerField** — `label`, `defaultValue`, `min`, `max`, `inclusive`, (`decimals` for DoubleField).
- **PercentField / CIField** — percent-specific shorthand; defaults appropriate for CIs.
- **TextField** — `defaultValue` or `placeholderText` (mutually exclusive), `afterLabel`, `fieldWidth`.
- **FormulaField** — adds `realValue`, `min/max`, `inclusive`, `multiple`, `realValues`.
- **TextArea** — `title`, `text`, `textType` (e.g., R code / JAGS / Lavaan / Model / Source), `separator(s)`, `applyScriptInfo` (submit with **Ctrl+Enter**).

### Variable specification
- **AvailableVariablesList** — `name`, `label`, **rich `source`** (other lists, levels, filters, `rSource`, combinations), or `values`; `width`, `count` (read-only).
- **AssignedVariablesList** — `name`, `label`, `allowedColumns`, `singleVariable`, `maxRows`, `listViewType` (e.g., `Interaction`), optional `rowComponent` (+ `rowComponentTitle`), `optionKey`, `count`.
- **FactorLevelList** — define RM factors/levels: `factorName`, `levelName`, `minFactors`, `minLevels`, `width`, `height`. Often paired with an `AssignedVariablesList` of type `MeasuresCells`.

### Complex composition
- **ComponentsList** — templated rows of controls from a `source` or `values`; `titles`, `rowComponent`, manual rows via `addItemManually`, bounds via `minimumItems` / `maximumItems`, collected under `optionKey`.
- **TabView** — `ComponentsList` rendered as tabs.
- **InputListView** — user adds rows via an input field; `title`, `placeHolder`, `defaultValues`, `minRows`, `inputComponent` (Text/Double/Integer), optional `rowComponent`, `optionKey`.
- **TableView** — `name`, `modelType` (`MultinomialChi2Model`, `JAGSDataInputModel`, `FilteredDataEntryModel`, `CustomContrasts`), `itemType` or per-row/column types, `source`; may override `getColHeaderText`, `getRowHeaderText`, `getDefaultValue`, `getValidator`.

### Grouping & structure
- **Group** — logical block with `title`, `columns`. Nest options inside.
- **Section** — collapsible panel for advanced options; `title`, `columns`. Use for lower-priority / expert settings.

## 4) Style & UX Conventions

- **Titles & labels:** Title Case for section/group titles; concise labels; every visible string uses `qsTr()`. The `name` is always the title transformed into camelCase. Options within groups inherit their names as a prefix.
- **Consistency:** Prefer the provided JASP controls over ad-hoc QML; nest subordinate options inside the control that enables them (e.g., a `CheckBox` containing its dependent fields).
- **Two-column rhythm:** Let the grid flow naturally; use `rowSpan/columnSpan` to avoid awkward gaps; avoid long single-column scrollers.
- **Variables first:** Place `VariablesForm` at the top; align list widths; restrict types with `allowedColumns`.
- **Defaults & placeholders:** Prefer meaningful `defaultValue`; use `placeholderText` only when input is optional. Don't set both.
- **Dropdowns:** Use `{label, value}` pairs when R-side value differs; add an explicit empty choice with `addEmptyValue` if "no selection" is valid.
- **Advanced options:** Tuck rare/expert settings into a `Section` titled "Advanced Options".
- **Docs:** Fill `info:` succinctly for every major control.
- **Spacing:** Always use tabs for spacing. Each argument on a new line. (See examples below.)

## 5) Quick Patterns

- **Enable dependent field(s):**
  ```qml
  CheckBox
  {
    id:     show
    name:   "showX"
    label:  qsTr("Show X")
  }

  DoubleField
  {
    name:         "Alpha"
    label:        qsTr("Alpha")
    defaultValue: 0.05
    min:          0
    max:          1
    decimals:     3
    enabled:      show.checked
  }
  ```

- **Radio choice with per-choice inputs:**
  ```qml
  RadioButtonGroup
  {
    name: "crit"
    title: qsTr("Criterion")

    RadioButton
    {
      value:      "pValue"
      label:      qsTr("p-value")
      checked:    true

      DoubleField
      {
        name:         "pValueValue"
        label:        ""
        defaultValue: 0.05
        min:          0
        max:          1
      }
    }

    RadioButton
    {
      ...
    }
  }
  ```

- **Variables form (single DV):**
  ```qml
  VariablesForm
  {
    AvailableVariablesList
    {
      name: "availableVariables"
    }

    AssignedVariablesList
    {
      name:           "dependentVariable"
      label:          qsTr("Dependent Variable")
      allowedColumns: ["scale"]
      singleVariable: true
    }
  }
  ```

## 6) When in doubt

- Prefer built-in JASP controls.
- Keep `name:` stable; translate strings; validate inputs.
- Put rare/expert options in a `Section` and document via `info:`.
