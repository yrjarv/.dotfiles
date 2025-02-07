// ==UserScript==
// @name         Belphegor - Devilry improved
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Make IFI's Devilry a little less confusing by adding more colour coding.
// @author       Johann Dahmen Tveranger
// @match        https://devilry.ifi.uio.no/*
// @exclude	 https://devilry.ifi.uio.no/
// @grant        none
// ==/UserScript==

(function() {
	"use strict";

	console.log("Belphegor enabled");


	const colorCodes = {
		"orange": "#fa7202",
		"yellow": "#d9f00e",
		"lightGreen": "#51f516",
		"darkGreen": "#01911c",
		"red": "#fc0505",
	};



	/** Returns true if the given HTML element contains an element with the given selector, otherwise false */
	function elementHasSelector(element, selector) {
		return (element.querySelector(selector) !== null);
	}

	/** Returns true if all function evaluate to true for the given object, else false */
	function checkAllConditions(obj, conditionList) {
		for (const conditionFunc of conditionList) {
			if (conditionFunc(obj) === false) {
				// console.log("Condition failed: " + conditionFunc.toString());
				return false;
			}
		}
		// console.log("All conditions passed" + conditionList.toString() + " for object: " + obj);
		return true;
	}

	// Applies some base css to the entire page to lay groundwork
	function setBaseCss() {
		const styleElement = document.createElement("style");
		const cssBaseRules = `

	       /* Sub-div of the main assignment boxes. Where background color is originally set, reset it to easily let the parent decide color */
		ol.cradmin-legacy-listbuilder-list li .cradmin-legacy-listbuilder-itemvalue.cradmin-legacy-listbuilder-itemvalue-focusbox
		{
			background-color: transparent !important;
		}
		/*Make assignment name, "grade passed" text, and comment summary have black text*/
		.devilry-body-student
		a.cradmin-legacy-listbuilder-itemframe-link
		.cradmin-legacy-listbuilder-itemvalue.cradmin-legacy-listbuilder-itemvalue-focusbox.cradmin-legacy-listbuilder-itemvalue-titledescription
		.cradmin-legacy-listbuilder-itemvalue-titledescription-title,
		.devilry-cradmin-groupitemvalue
		.devilry-cradmin-groupitemvalue-grade
		.devilry-core-grade-passed,
ol.cradmin-legacy-listbuilder-list p:last-child, ol.cradmin-legacy-listbuilder-list pre:last-child, ol.cradmin-legacy-listbuilder-list table:last-child, ol.cradmin-legacy-listbuilder-list form:last-child, ol.cradmin-legacy-listbuilder-list h2:last-child, ol.cradmin-legacy-listbuilder-list h3:last-child
		{
			color: black !important;
		}

		`;
		styleElement.textContent = cssBaseRules;
		document.head.appendChild(styleElement);
	}







	// Generates a button to open the Belphegor menu, as well as the menu itself
	function generateMenu() {

		function showMenu() {
			const menu = document.getElementById("belphegorMenu");
			menu.style.visibility = "visible";
		}

		function hideMenu() {
			const menu = document.getElementById("belphegorMenu");
			menu.style.visibility = "hidden";
		}

		function handleMenuButtonClick() {
			if (document.getElementById("belphegorMenu").style.visibility === "visible") {
				hideMenu();
			} else {
				showMenu();
			}
		}


		const parentElement = document.querySelector("div.container:nth-child(4) > h2:nth-child(1)");
		const belphegorUIContainer = document.createElement("div");
		belphegorUIContainer.setAttribute("id", "belphegorUIContainer");
		belphegorUIContainer.style.cssText = `
		font-size: 0.9em;
		margin: 0.5em;
		`;

		const menuButton = document.createElement("button");
		// v Add a line break before the button
		menuButton.textContent = "Belphegor";
		menuButton.setAttribute("id", "belphegorMenuButton");
		menuButton.addEventListener("click", handleMenuButtonClick);
		parentElement.appendChild(document.createElement("br"));
		menuButton.style.cssText = " background-color: red; display: none;";

		const menu = document.createElement("div");
		menu.setAttribute("id", "belphegorMenu");
		menu.style.cssText = "visibility: hidden; background-color: white; border: 2px solid black; width 15em; font-size: medium; font-weight: bold; position: absolute; z-index: 100;";
		const colorLegendHeader = document.createElement("span");
		colorLegendHeader.textContent = "Color legend";
		colorLegendHeader.style.cssText = "font-size: 1.2em; font-weight: bold; margin: 0.5em;";
		menu.appendChild(colorLegendHeader);

		for (const assignmentStatus in assignmentStatuses) {
			const statusDiv = document.createElement("div");
			statusDiv.setAttribute("class", "belphegorMenuStatusDiv");
			const { color, name } = assignmentStatuses[assignmentStatus];
			statusDiv.style.backgroundColor = color;
			statusDiv.style.width = "100%";
			statusDiv.style.height = "2em";
			statusDiv.style.padding = "0.5em";
			statusDiv.textContent = name;
			menu.appendChild(statusDiv);
		}

		parentElement.appendChild(belphegorUIContainer);
		belphegorUIContainer.appendChild(menuButton);
		belphegorUIContainer.appendChild(menu);
	}


	// Statuses for assignments. Conditions is list of arrow functions which all need to return true for the assignment object in order for it to qualify
	// as that satus. Color is the colour code associated with that status.
	const assignmentStatuses = {
		"waitingForDeliveriesNoFilesDelivered": {
			// If the assignment deadline is not yet reached and the first word in the studentfiles element is "0" (i.e. no files delivered)
			"conditions": [
				(obj) => elementHasSelector(obj, ".devilry-core-groupstatus-waiting-for-deliveries"),
				(obj) => obj.querySelector(".devilry-core-comment-summary-studentfiles").textContent.trim().split(" ")[0] === "0"
			],
			"color": colorCodes.orange,
			"name": "Waiting for deliveries (no files delivered)"
		},

		"waitingForDeliveriesFilesDelivered": {
			"conditions": [
				(obj) => elementHasSelector(obj, ".devilry-core-groupstatus-waiting-for-deliveries"),
			],
			"color": colorCodes.yellow,
			"name": "Waiting for deliveries (files delivered)"
		},
		"waitingForFeedback": {
			"conditions": [
				(obj) => elementHasSelector(obj, ".devilry-core-groupstatus-waiting-for-feedback"),
			],
			"color": colorCodes.lightGreen,
			"name": "Waiting for feedback"
		},
		"passed": {
			"conditions": [
				(obj) => elementHasSelector(obj, ".devilry-core-grade-passed"),
			],
			"color": colorCodes.darkGreen,
			"name": "Passed"
		},
		"failed": {
			"conditions": [
				(obj) => elementHasSelector(obj, ".devily-core-grade-failed"),
			],
			"color": colorCodes.red,
			"name": "Failed"
		}
	};






	// Returns all assignment elements on the page as an HTML collection
	function getAssignments() {
		// HTML collection of all assignment box elements (<a> tags)
		const assignmentCollection = document.getElementsByClassName("cradmin-legacy-listbuilder-itemframe");
		// console.log(assignmentCollection);
		return assignmentCollection;
	}


	function reColorAssignment(assignment) {
		for (const assignmentStatus in assignmentStatuses) {
			// console.log(assignmentStatus);
			if (checkAllConditions(assignment, assignmentStatuses[assignmentStatus]["conditions"])) {
				// console.log("THING GOT COLOURED WOOOO");
				assignment.style.setProperty(
					"background-color",
					assignmentStatuses[assignmentStatus]["color"],
					"important",
				);
				return;
			}
		}
	}

	// Recolours all given assignment elements (takes HTML collection)
	function recolorAllAssignments(assignments) {
		for (let i = 0; i < assignments.length; i++) {
			const assignment = assignments[i];
			reColorAssignment(assignment);
		}
	}

	generateMenu();
	setBaseCss();
	const assignments = getAssignments();
	recolorAllAssignments(assignments);




})();
