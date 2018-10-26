module Assets exposing (Assets, assets)

import Nri.Ui.AssetPath as AssetPath exposing (Asset(..))


type alias Assets =
    { activity : String
    , arrowDown : String
    , attention_svg : Asset
    , bulb : String
    , calendar : String
    , assignmentStartButtonPrimary_svg : Asset
    , assignmentStartButtonSecondary_svg : Asset
    , checkboxCheckedPartially_svg : Asset
    , checkboxChecked_svg : Asset
    , checkboxLockOnInside_svg : Asset
    , checkboxUnchecked_svg : Asset
    , checkmark : String
    , class : String
    , clever : String
    , clock : String
    , commentNotStarred_png : Asset
    , commentStarred_png : Asset
    , compass : String
    , darkBlueCheckmark_svg : Asset
    , diagnostic : String
    , document : String
    , download : String
    , edit : String
    , editWriting : String
    , exclamation : String
    , exclamationPoint_svg : Asset
    , facebookBlue_svg : Asset
    , flipper : String
    , footsteps : String
    , gear : String
    , hint_png : Asset
    , iconCalendar_svg : Asset
    , iconCheck_png : Asset
    , iconFlag_png : Asset
    , iconPremiumFlag_svg : Asset
    , iconPremiumWithWritingFlag_svg : Asset
    , icons_arrowDownBlue_svg : Asset
    , icons_arrowRightBlue_svg : Asset
    , icons_clockRed_svg : Asset
    , icons_equals_svg : Asset
    , icons_helpBlue_svg : Asset
    , icons_peerReview_svg : Asset
    , icons_peerReviewWhite_svg : Asset
    , icons_plusBlue_svg : Asset
    , icons_quickWrite_svg : Asset
    , icons_quickWriteWhite_svg : Asset
    , icons_searchGray_svg : Asset
    , icons_selfReviewWhite_svg : Asset
    , icons_xBlue_svg : Asset
    , icons_xBlue_svg : Asset
    , key : String
    , leaderboard : String
    , leftArrowBlue_png : Asset
    , level1Badge_png : Asset
    , level2Badge_png : Asset
    , level3Badge_png : Asset
    , lock : String
    , logoRedBlack_svg : Asset
    , masteryBadge : String
    , newspaper : String
    , openClose : String
    , peerReview : String
    , pen : Asset
    , performance : String
    , personBlue_svg : Asset
    , practice : String
    , premiumLock_svg : Asset
    , preview : String
    , quickWrite : String
    , quiz : String
    , rating : String
    , revising : String
    , seemore : String
    , selfReview : String
    , share : String
    , skip : String
    , smallCheckmark_png : Asset
    , sort : String
    , sortArrow : String
    , speedometer : String
    , squiggly_png : Asset
    , startingOffBadge_png : Asset
    , submitting : String
    , teach_assignments_copyWhite_svg : Asset
    , twitterBlue_svg : Asset
    , unarchiveBlue2x_png : Asset
    , writingAssignment : String
    , writingcycle : String
    , x : String
    , xWhite_svg : Asset
    }


assets : Assets
assets =
    { activity = "icon-activity"
    , arrowDown = "icon-arrow-down"
    , attention_svg = Asset "assets/images/attention.svg"
    , bulb = "icon-bulb"
    , calendar = "icon-calendar"
    , assignmentStartButtonPrimary_svg = Asset "assets/images/assignment-start-button-primary.svg"
    , assignmentStartButtonSecondary_svg = Asset "assets/images/assignment-start-button-secondary.svg"
    , checkboxCheckedPartially_svg = Asset "assets/images/checkbox_checkedPartially.svg"
    , checkboxChecked_svg = Asset "assets/images/checkbox_checked.svg"
    , checkboxLockOnInside_svg = Asset "assets/images/checkbox_lock_on_inside.svg"
    , checkboxUnchecked_svg = Asset "assets/images/checkbox_unchecked.svg"
    , checkmark = "icon-checkmark"
    , class = "icon-class"
    , clever = "icon-clever"
    , clock = "icon-clock"
    , commentNotStarred_png = Asset "assets/images/comment-notStarred.png"
    , commentStarred_png = Asset "assets/images/comment-starred.png"
    , compass = "icon-compass"
    , darkBlueCheckmark_svg = Asset "assets/images/dark-blue-checkmark.svg"
    , diagnostic = "icon-diagnostic"
    , document = "icon-document"
    , download = "icon-download"
    , edit = "icon-edit"
    , editWriting = "icon-edit-writing"
    , exclamation = "icon-exclamation"
    , exclamationPoint_svg = Asset "assets/images/exclamation-point.svg"
    , facebookBlue_svg = Asset "assets/images/facebook-blue.svg"
    , flipper = "icon-flipper"
    , footsteps = "icon-footsteps"
    , gear = "icon-gear"
    , hint_png = Asset "assets/images/hint.png"
    , iconCalendar_svg = Asset "assets/images/icon-calendar.svg"
    , iconCheck_png = Asset "assets/images/icon-check.png"
    , iconFlag_png = Asset "assets/images/icon-flag.png"
    , iconPremiumFlag_svg = Asset "assets/images/icon_premium_flag.svg"
    , iconPremiumWithWritingFlag_svg = Asset "assets/images/icon_premium_writing_flag.svg"
    , icons_arrowDownBlue_svg = Asset "assets/images/arrow-down-blue.svg"
    , icons_arrowRightBlue_svg = Asset "assets/images/arrow-right-blue.svg"
    , icons_clockRed_svg = Asset "assets/images/clock-red.svg"
    , icons_equals_svg = Asset "assets/images/equals.svg"
    , icons_helpBlue_svg = Asset "assets/images/help-blue.svg"
    , icons_peerReview_svg = Asset "assets/images/peer-review.svg"
    , icons_peerReviewWhite_svg = Asset "assets/images/peer-review-white.svg"
    , icons_plusBlue_svg = Asset "assets/images/plus-blue.svg"
    , icons_quickWrite_svg = Asset "assets/images/quick-write.svg"
    , icons_quickWriteWhite_svg = Asset "assets/images/quick-write-white.svg"
    , icons_searchGray_svg = Asset "assets/images/search-gray.svg"
    , icons_selfReviewWhite_svg = Asset "assets/images/self-review-white.svg"
    , icons_xBlue_svg = Asset "assets/images/x-blue.svg"
    , key = "icon-key"
    , leaderboard = "icon-leaderboard"
    , leftArrowBlue_png = Asset "assets/images/left-arrow-blue.png"
    , level1Badge_png = Asset "assets/images/level-1-badge.png"
    , level2Badge_png = Asset "assets/images/level-2-badge.png"
    , level3Badge_png = Asset "assets/images/level-3-badge.png"
    , lock = "icon-lock"
    , logoRedBlack_svg = Asset "assets/images/logo-red-black.svg"
    , masteryBadge = "icon-mastery-badge"
    , newspaper = "icon-newspaper"
    , openClose = "icon-open-close"
    , peerReview = "icon-peer-review"
    , pen = Asset "assets/images/pen.svg"
    , performance = "icon-performance"
    , personBlue_svg = Asset "assets/images/person-blue.svg"
    , practice = "icon-practice"
    , premiumLock_svg = Asset "assets/images/premium-lock.svg"
    , preview = "icon-preview"
    , quickWrite = "icon-quick-write"
    , quiz = "icon-quiz"
    , rating = "icon-rating"
    , revising = "icon-revising"
    , seemore = "icon-seemore"
    , selfReview = "icon-self-review"
    , share = "icon-share"
    , skip = "icon-skip"
    , smallCheckmark_png = Asset "assets/images/small_checkmark.png"
    , sort = "icon-sort"
    , sortArrow = "icon-sort-arrow"
    , speedometer = "icon-speedometer"
    , squiggly_png = Asset "assets/images/squiggly.png"
    , startingOffBadge_png = Asset "assets/images/starting-off-badge.png"
    , submitting = "icon-submitting"
    , teach_assignments_copyWhite_svg = Asset "assets/images/copy-white.svg"
    , twitterBlue_svg = Asset "assets/images/twitter-blue.svg"
    , unarchiveBlue2x_png = Asset "assets/images/unarchive-blue_2x.png"
    , writingAssignment = "icon-writing-assignment"
    , writingcycle = "icon-writingcycle"
    , x = "icon-x"
    , xWhite_svg = Asset "assets/images/x-white.svg"
    }
